module Hydra.Network.UDPSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Control.Concurrent.Class.MonadSTM (check, modifyTVar', newTVarIO)
import Hydra.Logging (showLogsOnFailure)
import Hydra.Network (Host (..), Network (..))
import Hydra.Network.UDP (PeersResolver, withUDPNetwork)
import Test.Network.Ports (randomUnusedTCPPorts)

spec :: Spec
spec = do
  let
    captureIncoming receivedMessages msg =
      atomically $ modifyTVar' receivedMessages (msg :)

  it "send and receive UDP messages" $ do
    [port1, port2] <- randomUnusedTCPPorts 2
    receivedByAlice <- newTVarIO mempty
    receivedByBob <- newTVarIO mempty
    let alice = Host "127.0.0.1" $ fromIntegral port1
        bob = Host "127.0.0.1" $ fromIntegral port2
        fromAlice = [1 .. 10 :: Int]
        fromBob = [11 .. 20 :: Int]

    showLogsOnFailure $ \tracer ->
      withUDPNetwork tracer alice (resolve [bob]) (captureIncoming receivedByAlice) $ \Network{broadcast = aliceSends} ->
        withUDPNetwork tracer bob (resolve [alice]) (captureIncoming receivedByBob) $ \Network{broadcast = bobSends} -> do
          forM_ fromAlice aliceSends
          forM_ fromBob bobSends

          failAfter 2 $
            concurrently_
              (receivedByAlice `shouldReceiveAll` fromBob)
              (receivedByBob `shouldReceiveAll` fromAlice)

shouldReceiveAll :: TVar IO [Int] -> [Int] -> IO ()
shouldReceiveAll var expected =
  atomically $ do
    received <- sort <$> readTVar var
    check $ received == expected

resolve :: [Host] -> PeersResolver IO a
resolve hosts = const $ pure hosts
