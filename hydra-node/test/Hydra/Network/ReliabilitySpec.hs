{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Hydra.Network.ReliabilitySpec where

import Hydra.Prelude hiding (empty, fromList, head, replicate, unlines)
import Test.Hydra.Prelude

import Control.Concurrent.Class.MonadSTM (
  MonadSTM (readTQueue, readTVarIO, writeTQueue),
  check,
  modifyTVar',
  newTQueueIO,
  newTVarIO,
  writeTVar,
 )
import Control.Monad.Class.MonadAsync (mapConcurrently_)
import Control.Monad.IOSim (runSimOrThrow)
import Control.Tracer (Tracer (..), nullTracer)
import Data.Map qualified as Map
import Data.Maybe qualified as Maybe
import Data.Sequence.Strict ((|>))
import Data.Vector (Vector, empty, fromList, head, replicate, snoc)
import Data.Vector qualified as Vector
import Hydra.Network (Network (..), NodeId (..))
import Hydra.Network.Authenticate (Authenticated (..))
import Hydra.Network.Heartbeat (Heartbeat (..), withHeartbeat)
import Hydra.Network.Message (Connectivity)
import Hydra.Network.Ouroboros (NetworkComponent)
import Hydra.Network.Reliability (MessagePersistence (..), ReliabilityLog (..), ReliableMsg (..), withReliability)
import Hydra.Node.Network (withFlipHeartbeats)
import Hydra.Party (Party)
import Hydra.Persistence (
  Persistence (..),
  PersistenceIncremental (..),
  createPersistence,
  createPersistenceIncremental,
 )
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import System.Random (StdGen, mkStdGen, uniformR)
import Test.Hydra.Fixture (Actor (..), actorName, actorParty, alice, bob, carol)
import Test.QuickCheck (
  Positive (Positive),
  collect,
  counterexample,
  generate,
  tabulate,
  within,
  (.&&.),
  (===),
 )
import Prelude (unlines)

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

    -- this test is quite critical as it demonstrates messages dropped are properly managed and resent to the
    -- other party whatever the length of queue, and whatever the interleaving of threads
    modifyMaxSuccess (const 5000) $
      prop "stress test networking layer" $ \(aliceToMessages :: [Int]) (bobToMessages :: [Int]) (carolToMessages :: [Int]) seed -> do
        let actorAToBMessagesMappings :: Map Actor [Int] =
              Map.fromList
                [ (Alice, aliceToMessages)
                , (Bob, bobToMessages)
                , (Carol, carolToMessages)
                ]
        let
          ([msgReceivedByAlice, msgReceivedByBob, msgReceivedByCarol], traces) = runSimOrThrow $ do
            emittedTraces <- newTVarIO []
            randomSeed <- newTVarIO $ mkStdGen seed

            let actors = [Alice, Bob, Carol]
            let connectedActors :: [(Actor, Actor)] = [(a, b) | a <- actors, b <- actors, a /= b]
            inboundMappings <- Map.fromList <$> mapM (\as -> newTQueueIO >>= (\tq -> pure (as, tq))) connectedActors
            outboundMappings <- Map.fromList <$> mapM (\as -> newTQueueIO >>= (\tq -> pure (as, tq))) connectedActors
            persistenceMappings <- Map.fromList <$> mapM (\a -> mockMessagePersistence 2 >>= (\p -> pure (a, p))) actors
            messagesReceivedByMappings <- Map.fromList <$> mapM (\a -> newTVarIO empty >>= (\tv -> pure (a, tv))) actors
            let createFailingNodePeer key@(actorA :: Actor, actorB :: Actor) =
                  let actorDriver :: Party = actorParty actorA
                      actorInbound = Maybe.fromJust $ Map.lookup key inboundMappings
                      actorOutbound = Maybe.fromJust $ Map.lookup key outboundMappings
                      actorPersistence = Maybe.fromJust $ Map.lookup actorA persistenceMappings
                      messagesReceivedByActorA = Maybe.fromJust $ Map.lookup actorA messagesReceivedByMappings
                      actorAToBMessages = Maybe.fromJust $ Map.lookup actorA actorAToBMessagesMappings
                      messagesReceivedByActorB = Maybe.fromJust $ Map.lookup actorB messagesReceivedByMappings
                      actorBToAMessages = Maybe.fromJust $ Map.lookup actorB actorAToBMessagesMappings
                      peerFailingNetwork = failingNetwork randomSeed actorDriver actorInbound actorOutbound
                      peerReliabilityStack = reliabilityStack actorPersistence peerFailingNetwork (captureTraces emittedTraces) (NodeId $ actorName actorA) actorDriver [actorParty actorB]
                   in runPeer peerReliabilityStack (NodeId $ actorName actorA) messagesReceivedByActorA messagesReceivedByActorB actorAToBMessages actorBToAMessages

            mapConcurrently_ createFailingNodePeer []

            logs <- readTVarIO emittedTraces

            let peerReceived (actor :: Actor) = do
                  let messagesReceivedByActor = Maybe.fromJust $ Map.lookup actor messagesReceivedByMappings
                  Vector.toList <$> readTVarIO messagesReceivedByActor

            peersReceivd <- mapM peerReceived actors
            pure (peersReceivd, logs)
         in
          within 1000000 $
            ( ( msgReceivedByAlice
                  === bobToMessages <> carolToMessages
              )
                .&&. ( msgReceivedByBob
                        === aliceToMessages <> carolToMessages
                     )
                .&&. ( msgReceivedByCarol
                        === aliceToMessages <> bobToMessages
                     )
            )
              & counterexample (unlines $ show <$> reverse traces)
              & tabulate "Messages from Bob to Alice" ["< " <> show ((length msgReceivedByAlice `div` 10 + 1) * 10)]
              & tabulate "Messages from Alice to Bob" ["< " <> show ((length msgReceivedByBob `div` 10 + 1) * 10)]
              & tabulate "Messages from Bob to Carol" ["< " <> show ((length msgReceivedByCarol `div` 10 + 1) * 10)]

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
  runPeer reliability partyName receivedMessageContainer sentMessageContainer messagesToSend expectedMessages =
    reliability (capturePayload receivedMessageContainer) $ \Network{broadcast} -> do
      forM_ messagesToSend $ \m -> do
        broadcast (Data partyName m)
        threadDelay 0.1

      concurrently_
        (waitForAllMessages expectedMessages receivedMessageContainer)
        (waitForAllMessages messagesToSend sentMessageContainer)

reliabilityStack ::
  (MonadThrow m, MonadThrow (STM m), MonadAsync m, MonadDelay m) =>
  MessagePersistence m outbound ->
  NetworkComponent
    m
    (Authenticated (ReliableMsg (Heartbeat inbound)))
    (ReliableMsg (Heartbeat outbound))
    a ->
  Tracer m ReliabilityLog ->
  NodeId ->
  Party ->
  [Party] ->
  NetworkComponent
    m
    (Either Connectivity (Authenticated inbound))
    outbound
    a
reliabilityStack persistence underlyingNetwork tracer nodeId party peers =
  withHeartbeat nodeId $
    withFlipHeartbeats $
      withReliability tracer persistence party peers underlyingNetwork
reloadAll :: FilePath -> IO [Heartbeat (Heartbeat String)]
reloadAll fileName =
  createPersistenceIncremental fileName
    >>= \PersistenceIncremental{loadAll} -> loadAll

failingNetwork ::
  MonadAsync m =>
  TVar m StdGen ->
  Party ->
  TQueue m inbound ->
  TQueue m (Authenticated outbound) ->
  NetworkComponent m inbound outbound a
failingNetwork seed peer readQueue writeQueue callback action =
  withAsync
    ( forever $ do
        newMsg <- atomically $ readTQueue readQueue
        dropPercent 0.02 seed $ callback newMsg
    )
    $ \_ ->
      action $
        Network
          { broadcast = \m ->
              dropPercent 0.02 seed $
                atomically $
                  writeTQueue writeQueue (Authenticated m peer)
          }

dropPercent :: MonadSTM m => Double -> TVar m StdGen -> m () -> m ()
dropPercent x seed f = do
  r <- randomNumber seed
  unless (r < x) f

randomNumber :: MonadSTM m => TVar m StdGen -> m Double
randomNumber seed' = do
  genSeed <- readTVarIO seed'
  let (res, newGenSeed) = uniformR (0 :: Double, 1) genSeed
  atomically $ writeTVar seed' newGenSeed
  pure res

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

failingMessagePersistence :: MonadSTM m => TVar m StdGen -> Int -> m (MessagePersistence m msg)
failingMessagePersistence seed numberOfParties = do
  acks <- newTVarIO $ replicate numberOfParties 0
  messages <- newTVarIO mempty
  pure $
    MessagePersistence
      { loadAcks = readTVarIO acks
      , saveAcks = dropPercent 0.02 seed . atomically . writeTVar acks
      , loadMessages = toList <$> readTVarIO messages
      , appendMessage = \msg -> dropPercent 0.02 seed $ atomically $ modifyTVar' messages (|> msg)
      }

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
