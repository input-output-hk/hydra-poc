{-# LANGUAGE DuplicateRecordFields #-}

module Hydra.Model.MockChain where

import Hydra.Cardano.Api
import Hydra.Prelude hiding (Any, label)

import Cardano.Binary (serialize', unsafeDeserialize')
import Control.Concurrent.Class.MonadSTM (
  MonadLabelledSTM,
  MonadSTM (newTVarIO, writeTVar),
  labelTQueueIO,
  labelTVarIO,
  modifyTVar,
  newTQueueIO,
  newTVarIO,
  readTVarIO,
  tryReadTQueue,
  writeTQueue,
  writeTVar,
 )
import Control.Monad.Class.MonadAsync (async, link)
import Control.Monad.Class.MonadFork (labelThisThread)
import Data.Sequence (Seq (Empty, (:|>)))
import qualified Data.Sequence as Seq
import Hydra.BehaviorSpec (
  SimulatedChainNetwork (..),
 )
import Hydra.Chain (Chain (..))
import Hydra.Chain.Direct (initialChainState)
import Hydra.Chain.Direct.Fixture (testNetworkId)
import Hydra.Chain.Direct.Handlers (
  ChainSyncHandler (..),
  DirectChainLog,
  LocalChainState,
  SubmitTx,
  chainSyncHandler,
  mkChain,
  newLocalChainState,
  onRollBackward,
  onRollForward,
 )
import Hydra.Chain.Direct.ScriptRegistry (genScriptRegistry)
import Hydra.Chain.Direct.State (ChainContext (..))
import qualified Hydra.Chain.Direct.State as S
import Hydra.Chain.Direct.TimeHandle (TimeHandle)
import Hydra.Chain.Direct.Wallet (TinyWallet (..))
import Hydra.ContestationPeriod (ContestationPeriod)
import Hydra.Crypto (HydraKey)
import Hydra.HeadLogic (
  Environment (Environment, otherParties, party),
  Event (..),
  defaultTTL,
 )
import Hydra.Logging (Tracer)
import Hydra.Model.Payment (CardanoSigningKey (..))
import Hydra.Network (Network (..))
import Hydra.Network.Message (Message)
import Hydra.Node (
  EventQueue (..),
  HydraNode (..),
  putEvent,
 )
import Hydra.Party (Party (..), deriveParty)

-- | Provide the logic to connect a list of `MockHydraNode` through a dummy chain.
mockChainAndNetwork ::
  forall m.
  ( MonadSTM m
  , MonadTimer m
  , MonadAsync m
  , MonadMask m
  , MonadThrow (STM m)
  , MonadLabelledSTM m
  , MonadFork m
  ) =>
  Tracer m DirectChainLog ->
  [(SigningKey HydraKey, CardanoSigningKey)] ->
  ContestationPeriod ->
  m (SimulatedChainNetwork Tx m)
mockChainAndNetwork tr seedKeys cp = do
  nodes <- newTVarIO []
  labelTVarIO nodes "nodes"
  queue <- newTQueueIO
  labelTQueueIO queue "chain-queue"
  chain <- newTVarIO (0, 0, Empty)
  tickThread <- async (labelThisThread "chain" >> simulateChain nodes queue chain)
  link tickThread
  pure
    SimulatedChainNetwork
      { rollbackAndForward = error "Not implemented, should never be called"
      , tickThread
      , connectNode = connectNode nodes queue
      }
 where
  connectNode nodes queue node = do
    let Environment{party = ownParty, otherParties} = env node
    let (vkey, vkeys) = findOwnCardanoKey ownParty seedKeys
    let ctx =
          S.ChainContext
            { networkId = testNetworkId
            , peerVerificationKeys = vkeys
            , ownVerificationKey = vkey
            , ownParty
            , otherParties
            , scriptRegistry = genScriptRegistry `generateWith` 42
            , contestationPeriod = cp
            }
    let getTimeHandle = pure $ arbitrary `generateWith` 42
    let seedInput = genTxIn `generateWith` 42
    let HydraNode{eq = EventQueue{putEvent}} = node
    localChainState <- newLocalChainState initialChainState
    let chainHandle =
          createMockChain
            tr
            ctx
            (atomically . writeTQueue queue)
            getTimeHandle
            seedInput
            localChainState
    let chainHandler =
          chainSyncHandler
            tr
            (putEvent . OnChainEvent)
            getTimeHandle
            ctx
            localChainState
    let node' =
          node
            { hn = createMockNetwork node nodes
            , oc = chainHandle
            }
    let mockNode = MockHydraNode{node = node', chainHandler}
    atomically $ modifyTVar nodes (mockNode :)
    pure node'

  blockTime :: Integer -- seconds
  blockTime = 20

  simulateChain nodes queue chain = forever $ do
    rollForward nodes chain queue
    rollForward nodes chain queue
    rollForward nodes chain queue
    rollForward nodes chain queue
    sendRollBackward nodes chain 2

  rollForward nodes chain queue = do
    threadDelay $ fromIntegral blockTime
    transactions <- flushQueue queue []
    addNewBlockToChain chain transactions
    sendRollForward nodes chain

  flushQueue queue transactions = do
    hasTx <- atomically $ tryReadTQueue queue
    case hasTx of
      Just tx -> do
        flushQueue queue (tx : transactions)
      Nothing -> pure transactions

  appendToChain block (slotNum, cursor, blocks) =
    (slotNum, cursor, blocks :|> block)

  sendRollForward nodes chain = do
    (slotNum, position, blocks) <- readTVarIO chain
    case Seq.lookup position blocks of
      Just (header, txs) -> do
        allHandlers <- fmap chainHandler <$> readTVarIO nodes
        forM_ allHandlers (\h -> onRollForward h header txs)
        atomically $ writeTVar chain (slotNum, position + 1, blocks)
      Nothing ->
        pure ()

  sendRollBackward nodes chain nbBlocks = do
    (slotNum, position, blocks) <- readTVarIO chain
    case Seq.lookup (position - nbBlocks) blocks of
      Just (header, _) -> do
        allHandlers <- fmap chainHandler <$> readTVarIO nodes
        let point = getChainPoint header
        forM_ allHandlers (`onRollBackward` point)
        atomically $ writeTVar chain (slotNum, position - nbBlocks + 1, blocks)
      Nothing ->
        pure ()

  addNewBlockToChain chain transactions =
    atomically $ modifyTVar chain $ \(slotNum, position, blocks) -> appendToChain (mkBlock transactions (fromIntegral $ slotNum + blockTime)) (slotNum + blockTime, position, blocks)

-- | Find Cardano vkey corresponding to our Hydra vkey using signing keys lookup.
-- This is a bit cumbersome and a tribute to the fact the `HydraNode` itself has no
-- direct knowlege of the cardano keys which are stored only at the `ChainComponent` level.
findOwnCardanoKey :: Party -> [(SigningKey HydraKey, CardanoSigningKey)] -> (VerificationKey PaymentKey, [VerificationKey PaymentKey])
findOwnCardanoKey me seedKeys = fromMaybe (error $ "cannot find cardano key for " <> show me <> " in " <> show seedKeys) $ do
  csk <- getVerificationKey . signingKey . snd <$> find ((== me) . deriveParty . fst) seedKeys
  pure (csk, filter (/= csk) $ map (getVerificationKey . signingKey . snd) seedKeys)

mkBlock :: [Tx] -> SlotNo -> (BlockHeader, [Tx])
mkBlock transactions slotNum =
  let header = genBlockHeaderAt slotNum `generateWith` 42
   in (header, transactions)

-- TODO: unify with BehaviorSpec's ?
createMockNetwork :: MonadSTM m => HydraNode Tx m -> TVar m [MockHydraNode m] -> Network m (Message Tx)
createMockNetwork myNode nodes =
  Network{broadcast}
 where
  broadcast msg = do
    allNodes <- fmap node <$> readTVarIO nodes
    let otherNodes = filter (\n -> getNodeId n /= getNodeId myNode) allNodes
    mapM_ (`handleMessage` msg) otherNodes

  handleMessage HydraNode{eq} = putEvent eq . NetworkEvent defaultTTL

  getNodeId = party . env

data MockHydraNode m = MockHydraNode
  { node :: HydraNode Tx m
  , chainHandler :: ChainSyncHandler m
  }

createMockChain ::
  (MonadTimer m, MonadThrow (STM m)) =>
  Tracer m DirectChainLog ->
  ChainContext ->
  SubmitTx m ->
  m TimeHandle ->
  TxIn ->
  LocalChainState m ->
  Chain Tx m
createMockChain tracer ctx submitTx timeHandle seedInput chainState =
  -- NOTE: The wallet basically does nothing
  let wallet =
        TinyWallet
          { getUTxO = pure mempty
          , getSeedInput = pure (Just seedInput)
          , sign = id
          , coverFee = \_ tx -> pure (Right tx)
          , reset = pure ()
          , update = \_ _ -> pure ()
          }
   in mkChain tracer timeHandle wallet ctx chainState submitTx

mkMockTxIn :: VerificationKey PaymentKey -> Word -> TxIn
mkMockTxIn vk ix = TxIn (TxId tid) (TxIx ix)
 where
  -- NOTE: Ugly, works because both binary representations are 32-byte long.
  tid = unsafeDeserialize' (serialize' vk)
