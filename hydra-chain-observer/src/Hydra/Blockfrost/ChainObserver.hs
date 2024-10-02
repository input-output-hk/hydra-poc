{-# LANGUAGE DuplicateRecordFields #-}

module Hydra.Blockfrost.ChainObserver where

import Hydra.Prelude

import Blockfrost.Client (
  BlockfrostClientT,
  runBlockfrost,
 )
import Blockfrost.Client qualified as Blockfrost
import Control.Concurrent.Class.MonadSTM (
  MonadSTM (readTVarIO),
  newTVarIO,
  writeTVar,
 )
import Control.Retry (constantDelay, retrying)
import Hydra.Cardano.Api (
  BlockHeader,
  ChainPoint (..),
  HasTypeProxy (..),
  Hash,
  NetworkId (..),
  NetworkMagic (..),
  SerialiseAsCBOR (..),
  SlotNo (..),
  Tx,
  UTxO,
 )
import Hydra.Chain.Direct.Handlers (convertObservation)
import Hydra.ChainObserver.NodeClient (
  ChainObservation (..),
  ChainObserverLog (..),
  NodeClient (..),
  ObserverHandler,
  logOnChainTx,
  observeAll,
 )
import Hydra.Logging (Tracer, traceWith)
import Hydra.Tx (IsTx (..))

data APIBlockfrostError
  = BlockfrostError Text
  | DecodeError Text
  | NotEnoughBlockConfirmations Blockfrost.BlockHash
  | MissingBlockNo Blockfrost.BlockHash
  | MissingNextBlockHash Blockfrost.BlockHash
  deriving (Show, Exception)

runBlockfrostM ::
  (MonadIO m, MonadThrow m) =>
  Blockfrost.Project ->
  BlockfrostClientT IO a ->
  m a
runBlockfrostM prj action = do
  result <- liftIO $ runBlockfrost prj action
  case result of
    Left err -> throwIO (BlockfrostError $ show err)
    Right val -> pure val

blockfrostClient ::
  Tracer IO ChainObserverLog ->
  FilePath ->
  Integer ->
  NodeClient IO
blockfrostClient tracer projectPath blockConfirmations = do
  NodeClient
    { follow = \_ startChainFrom observerHandler -> do
        prj <- Blockfrost.projectFromFile projectPath

        Blockfrost.Block{_blockHash = (Blockfrost.BlockHash genesisBlockHash)} <-
          runBlockfrostM prj (Blockfrost.getBlock (Left 0))

        Blockfrost.Genesis
          { _genesisActiveSlotsCoefficient
          , _genesisSlotLength
          , _genesisNetworkMagic
          } <-
          runBlockfrostM prj Blockfrost.getLedgerGenesis

        let networkId = fromNetworkMagic _genesisNetworkMagic
        traceWith tracer ConnectingToExternalNode{networkId}

        chainPoint <-
          case startChainFrom of
            Just point -> pure point
            Nothing -> do
              toChainPoint <$> runBlockfrostM prj Blockfrost.getLatestBlock

        traceWith tracer StartObservingFrom{chainPoint}

        let blockTime = realToFrac _genesisSlotLength / realToFrac _genesisActiveSlotsCoefficient

        let blockHash = fromChainPoint chainPoint genesisBlockHash

        stateTVar <- newTVarIO (blockHash, mempty)
        void $
          retrying (retryPolicy blockTime) shouldRetry $ \_ -> do
            loop tracer prj networkId blockTime observerHandler stateTVar
    }
 where
  shouldRetry _ = \case
    Right{} -> pure False
    Left err -> pure $ isRetryable (spy err)

  retryPolicy blockTime = constantDelay (truncate blockTime * 1000 * 1000)

-- | Iterative process that follows the chain using a naive roll-forward approach,
-- keeping track of the latest known current block and UTxO view.
-- This process operates at full speed without waiting between calls,
-- favoring the catch-up process.
loop ::
  (MonadIO m, MonadThrow m, MonadSTM m) =>
  Tracer m ChainObserverLog ->
  Blockfrost.Project ->
  NetworkId ->
  DiffTime ->
  ObserverHandler m ->
  TVar m (Blockfrost.BlockHash, UTxO) ->
  m a
loop tracer prj networkId blockTime observerHandler stateTVar = do
  current <- readTVarIO stateTVar
  next <- rollForward tracer prj networkId observerHandler current
  atomically $ writeTVar stateTVar next
  loop tracer prj networkId blockTime observerHandler stateTVar

-- | From the current block and UTxO view, we collect Hydra observations
-- and yield the next block and adjusted UTxO view.
rollForward ::
  (MonadIO m, MonadThrow m) =>
  Tracer m ChainObserverLog ->
  Blockfrost.Project ->
  NetworkId ->
  ObserverHandler m ->
  (Blockfrost.BlockHash, UTxO) ->
  m (Blockfrost.BlockHash, UTxO)
rollForward tracer prj networkId observerHandler (blockHash, utxo) = do
  block@Blockfrost.Block
    { _blockHash
    , _blockConfirmations
    , _blockNextBlock
    , _blockHeight
    } <-
    runBlockfrostM prj $ Blockfrost.getBlock (Right blockHash)

  -- Check if block within the safe zone to be processes
  -- FIXME: should be configurable
  when (_blockConfirmations < 1) $
    throwIO (NotEnoughBlockConfirmations _blockHash)

  -- Search block transactions
  txHashes <- runBlockfrostM prj . Blockfrost.allPages $ \p ->
    Blockfrost.getBlockTxs' (Right _blockHash) p Blockfrost.def

  -- Collect CBOR representations
  cborTxs <- traverse (runBlockfrostM prj . Blockfrost.getTxCBOR) txHashes

  -- Convert to cardano-api Tx
  receivedTxs <- mapM toTx cborTxs
  let receivedTxIds = txId <$> receivedTxs
  let point = toChainPoint block
  traceWith tracer RollForward{point, receivedTxIds}

  -- Collect head observations
  let (adjustedUTxO, observations) = observeAll networkId utxo receivedTxs
  let onChainTxs = mapMaybe convertObservation observations
  forM_ onChainTxs (traceWith tracer . logOnChainTx)

  blockNo <- maybe (throwIO $ MissingBlockNo _blockHash) (pure . fromInteger) _blockHeight
  let observationsAt = HeadObservation point blockNo <$> onChainTxs

  -- Call observer handler
  observerHandler $
    if null observationsAt
      then [Tick point blockNo]
      else observationsAt

  -- Next
  case _blockNextBlock of
    Just nextBlockHash ->
      pure (nextBlockHash, adjustedUTxO)
    Nothing ->
      -- FIXME: should not error (and retry) after observing already
      throwIO (MissingNextBlockHash _blockHash)

-- * Helpers

isRetryable :: APIBlockfrostError -> Bool
isRetryable (BlockfrostError _) = True
isRetryable (DecodeError _) = False
isRetryable (NotEnoughBlockConfirmations _) = True
isRetryable (MissingBlockNo _) = True
isRetryable (MissingNextBlockHash _) = True

toChainPoint :: Blockfrost.Block -> ChainPoint
toChainPoint Blockfrost.Block{_blockSlot, _blockHash} =
  ChainPoint slotNo headerHash
 where
  slotNo :: SlotNo
  slotNo = maybe 0 (fromInteger . Blockfrost.unSlot) _blockSlot

  headerHash :: Hash BlockHeader
  headerHash = fromString . toString $ Blockfrost.unBlockHash _blockHash

fromNetworkMagic :: Integer -> NetworkId
fromNetworkMagic = \case
  0 -> Mainnet
  magicNbr -> Testnet (NetworkMagic (fromInteger magicNbr))

toTx :: MonadThrow m => Blockfrost.TransactionCBOR -> m Tx
toTx (Blockfrost.TransactionCBOR txCbor) =
  case decodeBase16 txCbor of
    Left decodeErr -> throwIO . DecodeError $ "Bad Base16 Tx CBOR: " <> decodeErr
    Right bytes ->
      case deserialiseFromCBOR (proxyToAsType (Proxy @Tx)) bytes of
        Left deserializeErr -> throwIO . DecodeError $ "Bad Tx CBOR: " <> show deserializeErr
        Right tx -> pure tx

fromChainPoint :: ChainPoint -> Text -> Blockfrost.BlockHash
fromChainPoint chainPoint genesisBlockHash = case chainPoint of
  ChainPoint _ headerHash -> Blockfrost.BlockHash $ show headerHash
  ChainPointAtGenesis -> Blockfrost.BlockHash genesisBlockHash
