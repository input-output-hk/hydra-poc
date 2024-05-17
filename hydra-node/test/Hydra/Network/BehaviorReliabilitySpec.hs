module Hydra.Network.BehaviorReliabilitySpec where

import Hydra.Prelude hiding (empty, fromList, head, replicate, unlines)

import Control.Concurrent.Class.MonadSTM (
  MonadLabelledSTM,
  MonadSTM (readTVarIO),
  modifyTVar,
  newTQueue,
  newTVarIO,
 )
import Control.Monad.Class.MonadAsync (mapConcurrently_)
import Control.Tracer (nullTracer)
import Hydra.API.ClientInput (ClientInput (..))
import Hydra.API.Server (Server (..))
import Hydra.BehaviorSpec (TestHydraClient (..), createTestHydraClient)
import Hydra.Chain (Chain (..), initHistory)
import Hydra.Environment (Environment)
import Hydra.Events (EventSource (..))
import Hydra.HeadLogic (Input (..), defaultTTL)
import Hydra.HeadLogic.State (CoordinatedHeadState (..), HeadState (..), OpenState (..))
import Hydra.HeadLogicSpec (inOpenState)
import Hydra.Ledger (ChainSlot (..))
import Hydra.Ledger.Simple (SimpleChainState (..), SimpleTx (..), simpleLedger)
import Hydra.Network (Host (..), Network (..), NodeId (..))
import Hydra.Network.Message (Message, NetworkEvent (..))
import Hydra.Node (DraftHydraNode (..), HydraNode (..), connect, createNodeState, initEnvironment, runHydraNode)
import Hydra.Node.InputQueue (InputQueue (..), createInputQueue)
import Hydra.Options (RunOptions (..), defaultRunOptions)
import Hydra.Party (HasParty (..), Party, getParty)
import Test.Hydra.Fixture (alice, bob, carol)

-- TODO: drop messages on broadcast and on handleMessage
createMockNetwork :: MonadSTM m => DraftHydraNode tx m -> TVar m [HydraNode tx m] -> Network m (Message tx)
createMockNetwork node nodes =
  Network{broadcast}
 where
  broadcast msg = do
    allNodes <- readTVarIO nodes
    let otherNodes = filter (\n -> getParty n /= getParty node) allNodes
    mapM_ (`handleMessage` msg) otherNodes

  handleMessage HydraNode{inputQueue} msg =
    enqueue inputQueue . NetworkInput defaultTTL $ ReceivedMessage{sender, msg}

  sender = getParty node

connectNode :: MonadSTM m => DraftHydraNode tx m -> TVar m [HydraNode tx m] -> m ()
connectNode draftNode nodes = do
  let
    mockChain =
      Chain
        { postTx = \_ -> error "unexpected call to postTxTx"
        , draftCommitTx = \_ -> error "unexpected call to draftCommitTx"
        , submitTx = \_ -> error "unexpected call to submitTx"
        }
    mockNetwork = createMockNetwork draftNode nodes
    mockServer = Server{sendOutput = const $ pure ()}
  node <- connect mockChain mockNetwork mockServer draftNode
  atomically $ modifyTVar nodes (node :)
  pure ()

hydrateNode :: (MonadLabelledSTM m, MonadDelay m, MonadAsync m) => [Party] -> Environment -> m (DraftHydraNode SimpleTx m)
hydrateNode parties env = do
  nodeState <- createNodeState Nothing (inOpenState parties)
  inputQueue <- createInputQueue
  pure $
    DraftHydraNode
      { tracer = Control.Tracer.nullTracer
      , env
      , ledger = simpleLedger
      , nodeState
      , inputQueue
      , eventSource = EventSource{getEvents = pure []}
      , eventSinks = []
      , chainStateHistory = initHistory SimpleChainState{slot = ChainSlot 0}
      }

setup = do
  tvNodes <- newTVarIO []
  forM_
    [aliceOptions, bobOptions, carolOptions]
    ( \opt -> do
        env <- initEnvironment opt
        wetHydraNode <- hydrateNode [alice, bob, carol] env
        connectNode wetHydraNode tvNodes
    )

  nodes <- readTVarIO tvNodes

  outputs <- atomically newTQueue
  outputHistory <- newTVarIO mempty

  mapConcurrently_
    ( \node -> do
        withAsync (runHydraNode node) $ \_ -> do
          -- FIXME we want nodes to run and send a fixed list of inputs
          let TestHydraClient{send, queryState} = createTestHydraClient outputs outputHistory node
          headState <- queryState
          case headState of
            Open (OpenState{coordinatedHeadState = CoordinatedHeadState{localUTxO}}) ->
              send $ NewTx SimpleTx{}
            _ -> pure ()
          pure ()
    )
    nodes
 where
  aliceOptions =
    defaultRunOptions
      { nodeId = NodeId "hydra-node-1"
      , port = 5001
      , peers = [Host{hostname = "127.0.0.1", port = 5002}, Host{hostname = "127.0.0.1", port = 5003}]
      , apiPort = 4001
      , hydraSigningKey = "alice.sk"
      , hydraVerificationKeys = ["bob.vk", "carol.vk"]
      , persistenceDir = "./alice-state/"
      }

  bobOptions =
    defaultRunOptions
      { nodeId = NodeId "hydra-node-2"
      , port = 5002
      , peers = [Host{hostname = "127.0.0.1", port = 5001}, Host{hostname = "127.0.0.1", port = 5003}]
      , apiPort = 4002
      , hydraSigningKey = "bob.sk"
      , hydraVerificationKeys = ["alice.vk", "carol.vk"]
      , persistenceDir = "./bob-state/"
      }

  carolOptions =
    defaultRunOptions
      { nodeId = NodeId "hydra-node-3"
      , port = 5003
      , peers = [Host{hostname = "127.0.0.1", port = 5001}, Host{hostname = "127.0.0.1", port = 5002}]
      , apiPort = 4003
      , hydraSigningKey = "carol.sk"
      , hydraVerificationKeys = ["alice.vk", "bob.vk"]
      , persistenceDir = "./carol-state/"
      }
