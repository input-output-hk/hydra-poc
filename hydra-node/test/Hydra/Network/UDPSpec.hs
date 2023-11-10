{-# LANGUAGE CPP #-}

module Hydra.Network.UDPSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Control.Concurrent.Class.MonadSTM (
  check,
  modifyTVar',
  newTVarIO,
  readTVarIO,
 )
import Hydra.Logging (showLogsOnFailure)
import Hydra.Network (Host (..), Network (..))
import Hydra.Network.UDP (PeersResolver, UDPMsg (..), withUDPNetwork)
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
        fromAliceToBob = UDPMsg (Just bob) <$> [1 .. 10 :: Int]
        fromBobToAlice = UDPMsg (Just alice) <$> [11 .. 20 :: Int]
        fromBobAsAlice = UDPMsg (Just bob) <$> [11 .. 20 :: Int]
        fromAliceAsBob = UDPMsg (Just alice) <$> [1 .. 10 :: Int]

    showLogsOnFailure $ \tracer ->
      withUDPNetwork tracer alice (captureIncoming receivedByAlice) $ \Network{broadcast = aliceSends} ->
        withUDPNetwork tracer bob (captureIncoming receivedByBob) $ \Network{broadcast = bobSends} -> do
          forM_ fromAliceToBob aliceSends
          forM_ fromBobToAlice bobSends

          res <-
            timeout 2 $
              concurrently_
                (receivedByAlice `shouldReceiveAll` fromBobAsAlice)
                (receivedByBob `shouldReceiveAll` fromAliceAsBob)
          case res of
            Nothing -> do
              aliceRcvd <- readTVarIO receivedByAlice
              bobRcvd <- readTVarIO receivedByBob
              failure $
                "Received by alice ("
                  <> show alice
                  <> "): "
                  <> show (sort aliceRcvd)
                  <> "\nReceived by bob ("
                  <> show bob
                  <> "): "
                  <> show (sort bobRcvd)
            Just () -> pure ()

shouldReceiveAll :: Ord a => TVar IO [a] -> [a] -> IO ()
shouldReceiveAll var (sort -> expected) =
  atomically $ do
    received <- sort <$> readTVar var
    check $ received == expected

resolve :: [Host] -> PeersResolver IO a
resolve hosts = const $ pure hosts
