module Hydra.Network.ReliabilitySpec where

import Hydra.Prelude hiding (empty, fromList, head, replicate, unlines)
import Test.Hydra.Prelude

import Control.Concurrent.Class.MonadSTM (
  MonadSTM (readTVarIO),
  check,
  modifyTVar',
  newTVarIO,
  writeTVar,
 )
import Control.Monad.IOSim (runSimOrThrow)
import Control.Tracer (Tracer (..), nullTracer)
import Data.Map qualified as Map
import Data.Sequence.Strict ((|>))
import Data.Vector (Vector, empty, fromList, head, replicate, snoc)
import Data.Vector qualified as Vector
import Hydra.Network (Network (..), NewNetwork (..), NodeId (..), callback, newCallback, setCallback)
import Hydra.Network.Authenticate (Authenticated (..))
import Hydra.Network.Heartbeat (Heartbeat (..), newHeartbeat)
import Hydra.Network.Message (Connectivity)
import Hydra.Network.Reliability (MessagePersistence (..), ReliabilityLog (..), ReliableMsg (..), newReliability, withReliability)
import Hydra.Node.Network (newFlipHeartbeats)
import Hydra.NodeSpec (messageRecorder)
import Hydra.Party (Party)
import Hydra.Persistence (
  Persistence (..),
  PersistenceIncremental (..),
  createPersistence,
  createPersistenceIncremental,
 )
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import System.Random (mkStdGen, uniformR)
import Test.Hydra.Fixture (alice, bob, carol)
import Test.QuickCheck (
  Positive (Positive),
  Property,
  collect,
  counterexample,
  generate,
  tabulate,
  within,
  (===),
 )

spec :: Spec
spec = parallel $ do
  let captureOutgoing msgqueue _callback action =
        action $ Network{broadcast = \msg -> atomically $ modifyTVar' msgqueue (`snoc` msg)}

  let msg' = 42 :: Int
  msg <- Data "node-1" <$> runIO (generate @String arbitrary)

  describe "receiving messages" $ do
    it "forward received messages" $ do
      let propagatedMessages =
            aliceReceivesMessages
              [Authenticated (ReliableMsg (fromList [1, 1, 1]) (Data "node-2" msg)) bob]

      propagatedMessages `shouldBe` [Authenticated (Data "node-2" msg) bob]

    it "do not drop messages with same ids from different peers" $ do
      let propagatedMessages =
            aliceReceivesMessages
              [ Authenticated (ReliableMsg (fromList [0, 1, 0]) (Data "node-2" msg')) bob
              , Authenticated (ReliableMsg (fromList [0, 0, 1]) (Data "node-3" msg')) carol
              ]

      propagatedMessages `shouldBe` [Authenticated (Data "node-2" msg') bob, Authenticated (Data "node-3" msg') carol]

    it "Ignores messages with malformed acks" $ do
      let malFormedAck = fromList [1, 0]
          wellFormedAck = fromList [1, 0, 1]
          propagatedMessages =
            aliceReceivesMessages
              [ Authenticated (ReliableMsg malFormedAck (Data "node-2" msg')) bob
              , Authenticated (ReliableMsg wellFormedAck (Data "node-3" msg')) carol
              ]

      propagatedMessages `shouldBe` [Authenticated (Data "node-3" msg') carol]

    prop "drops already received messages" $ \(messages :: [Positive Int]) ->
      let
        messagesToSend =
          (\(Positive m) -> Authenticated (ReliableMsg (fromList [0, m, 0]) (Data "node-2" m)) bob)
            <$> messages
        propagatedMessages = aliceReceivesMessages messagesToSend

        receivedMessagesInOrder messageReceived =
          let refMessages = Data "node-2" <$> [1 ..]
              isInMessage Authenticated{payload} = payload `elem` refMessages
           in all isInMessage messageReceived
       in
        receivedMessagesInOrder propagatedMessages
          & counterexample (show propagatedMessages)
          & collect (length propagatedMessages)

  describe "sending messages" $ do
    prop "broadcast messages to the network assigning a sequential id" $ \(messages :: [String]) ->
      let sentMsgs = runSimOrThrow $ do
            sentMessages <- newTVarIO empty
            persistence <- mockMessagePersistence 1

            withReliability nullTracer persistence alice [] (captureOutgoing sentMessages) noop $ \Network{broadcast} -> do
              mapM_ (broadcast . Data "node-1") messages

            fromList . Vector.toList <$> readTVarIO sentMessages
       in head . knownMessageIds <$> sentMsgs `shouldBe` fromList [1 .. (length messages)]

    prop "stress test networking layer" prop_stressTest

    it "broadcast updates counter from peers" $ do
      let receivedMsgs = runSimOrThrow $ do
            sentMessages <- newTVarIO empty
            alicePersistence <- mockMessagePersistence 2
            withReliability
              nullTracer
              alicePersistence
              alice
              [bob]
              ( \incoming action -> do
                  concurrently_
                    (action $ Network{broadcast = \m -> atomically $ modifyTVar' sentMessages (`snoc` m)})
                    (incoming (Authenticated (ReliableMsg (fromList [0, 1]) (Data "node-2" msg)) bob))
              )
              noop
              $ \Network{broadcast} -> do
                threadDelay 1
                broadcast (Data "node-1" msg)
            Vector.toList <$> readTVarIO sentMessages

      receivedMsgs `shouldBe` [ReliableMsg (fromList [1, 1]) (Data "node-1" msg)]

    it "appends messages to disk and can load them back" $ do
      withTempDir "network-messages-persistence" $ \tmpDir -> do
        let networkMessagesFile = tmpDir <> "/network-messages"

        Persistence{load, save} <- createPersistence $ tmpDir <> "/acks"
        PersistenceIncremental{loadAll, append} <- createPersistenceIncremental networkMessagesFile

        let messagePersistence =
              MessagePersistence
                { loadAcks = do
                    mloaded <- load
                    case mloaded of
                      Nothing -> pure $ replicate (length [alice, bob]) 0
                      Just acks -> pure acks
                , saveAcks = save
                , loadMessages = loadAll
                , appendMessage = append
                }

        receivedMsgs <- do
          sentMessages <- newTVarIO empty
          withReliability
            nullTracer
            messagePersistence
            alice
            [bob]
            ( \incoming action -> do
                concurrently_
                  (action $ Network{broadcast = \m -> atomically $ modifyTVar' sentMessages (`snoc` m)})
                  (incoming (Authenticated (ReliableMsg (fromList [0, 1]) (Data "node-2" msg)) bob))
            )
            noop
            $ \Network{broadcast} -> do
              threadDelay 1
              broadcast (Data "node-1" msg)
          Vector.toList <$> readTVarIO sentMessages

        receivedMsgs `shouldBe` [ReliableMsg (fromList [1, 1]) (Data "node-1" msg)]

        doesFileExist networkMessagesFile `shouldReturn` True
        reloadAll networkMessagesFile `shouldReturn` [Data "node-1" msg]

        doesFileExist (tmpDir </> "acks") `shouldReturn` True
        load `shouldReturn` Just (fromList [1, 1])
 where
  reloadAll :: FilePath -> IO [Heartbeat (Heartbeat String)]
  reloadAll fileName =
    createPersistenceIncremental fileName
      >>= \PersistenceIncremental{loadAll} -> loadAll

noop :: Monad m => b -> m ()
noop = const $ pure ()

aliceReceivesMessages :: [Authenticated (ReliableMsg (Heartbeat msg))] -> [Authenticated (Heartbeat msg)]
aliceReceivesMessages messages = runSimOrThrow $ do
  receivedMessages <- newTVarIO empty
  alicePersistence <- mockMessagePersistence 3

  let baseNetwork incoming _ = mapM incoming messages

      aliceReliabilityStack =
        withReliability
          nullTracer
          alicePersistence
          alice
          [bob, carol]
          baseNetwork

  void $ aliceReliabilityStack (captureIncoming receivedMessages) $ \_action ->
    pure [()]

  Vector.toList <$> readTVarIO receivedMessages

captureIncoming :: MonadSTM m => TVar m (Vector p) -> p -> m ()
captureIncoming receivedMessages msg =
  atomically $ modifyTVar' receivedMessages (`snoc` msg)

capturePayload :: MonadSTM m => TVar m (Vector msg) -> Either Connectivity (Authenticated (Heartbeat msg)) -> m ()
capturePayload receivedMessages = \case
  Right Authenticated{payload = Data _ msg} ->
    atomically $ modifyTVar' receivedMessages (`snoc` msg)
  _ -> pure ()

waitForAllMessages :: MonadSTM m => [msg] -> TVar m (Vector msg) -> m ()
waitForAllMessages expectedMessages capturedMessages = atomically $ do
  msgs <- readTVar capturedMessages
  check $ length msgs == length expectedMessages

captureTraces ::
  MonadSTM m =>
  TVar m [ReliabilityLog] ->
  Tracer m ReliabilityLog
captureTraces tvar = Tracer $ \msg -> do
  atomically $ modifyTVar' tvar (msg :)

mockMessagePersistence :: Int -> MonadSTM m => m (MessagePersistence m msg)
mockMessagePersistence numberOfParties = do
  acks <- newTVarIO $ replicate numberOfParties 0
  messages <- newTVarIO mempty
  pure $
    MessagePersistence
      { loadAcks = readTVarIO acks
      , saveAcks = atomically . writeTVar acks
      , loadMessages = toList <$> readTVarIO messages
      , appendMessage = \msg -> atomically $ modifyTVar' messages (|> msg)
      }

prop_stressTest :: [Int] -> [Int] -> Int -> Property
prop_stressTest aliceMessages bobMessages seed =
  within 1000000 $
    msgReceivedByBob === aliceMessages
      -- TODO: capture and show traces & counterexample (unlines $ show <$> reverse traces)
      & counterexample "Bob received messages not matching Alice's sent messages."
      & tabulate "Messages from Alice to Bob" ["< " <> show ((length msgReceivedByBob `div` 10 + 1) * 10)]
      & tabulate "Messages from Bob to Alice" ["< " <> show ((length msgReceivedByAlice `div` 10 + 1) * 10)]
 where
  (msgReceivedByAlice, msgReceivedByBob) = runSimOrThrow $ do
    connect <- createSometimesFailingNetwork
    (recordAliceMessage, getAliceMessages) <- messageRecorder
    aliceNetwork <- connect alice
    -- aliceNetwork <- reliableNetwork alice [bob] =<< connect alice
    setCallback (onMessageReceived aliceNetwork) recordAliceMessage

    (recordBobMessage, getBobMessages) <- messageRecorder
    bobNetwork <- connect bob
    -- bobNetwork <- reliableNetwork bob [alice] =<< connect bob
    setCallback (onMessageReceived bobNetwork) recordBobMessage

    sendAll aliceNetwork aliceMessages
      `concurrently_` sendAll bobNetwork bobMessages

    (,) <$> getAliceMessages <*> (onlyData <$> getBobMessages)

  onlyData = map (\Authenticated{payload} -> payload)

  createSometimesFailingNetwork :: MonadSTM m => m (Party -> m (NewNetwork m (Authenticated msg) msg))
  createSometimesFailingNetwork = do
    stdGenV <- newTVarIO $ mkStdGen seed
    peerMapV <- newTVarIO mempty
    pure $ \party -> do
      onMessageReceived <- newCallback
      let receiveMessage msg = do
            callback onMessageReceived msg
      atomically $ modifyTVar' peerMapV (Map.insert party receiveMessage)
      pure
        NewNetwork
          { broadcast = \msg -> do
              peers <- readTVarIO peerMapV
              forM_ (Map.toList peers) $ \(p, send) -> do
                -- drop 2% of messages
                r <- randomNumber stdGenV
                unless (p == party && r < 1) $
                  send (Authenticated msg party) -- calls receiveMessage on the other end
          , onMessageReceived
          }

  randomNumber stdGenV = atomically $ do
    genSeed <- readTVar stdGenV
    let (res, newGenSeed) = uniformR (0 :: Double, 1) genSeed
    writeTVar stdGenV newGenSeed
    pure res

  sendAll NewNetwork{broadcast} msgs =
    forM_ msgs $ \m -> do
      broadcast m
      threadDelay 0.1

-- | Implementation of the reliability stack using the 'NewNetwork' interface
reliableNetwork ::
  MonadSTM m =>
  Party ->
  [Party] ->
  NewNetwork m (Authenticated (ReliableMsg (Heartbeat inbound))) (ReliableMsg (Heartbeat outbound)) ->
  m (NewNetwork m (Either Connectivity (Authenticated inbound)) outbound)
reliableNetwork party peers underlyingNetwork = do
  persistence <- mockMessagePersistence (length peers)
  newHeartbeat nodeId
    =<< newFlipHeartbeats
    =<< newReliability nullTracer persistence party peers underlyingNetwork
 where
  nodeId = NodeId (show party)
