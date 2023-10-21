{-# LANGUAGE CPP #-}

module Hydra.Network.UDPSpec where

import Hydra.Prelude hiding (show)
import Test.Hydra.Prelude

import Control.Concurrent.Class.MonadSTM (
  check,
  modifyTVar',
  newEmptyTMVar,
  newTVarIO,
  putTMVar,
  takeTMVar,
 )
import Data.Aeson (Value (String))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Hex
import Hydra.Logging (showLogsOnFailure)
import Hydra.Network (Host (..), Network (..))
import Hydra.Network.ReliabilitySpec (noop)
import Hydra.Network.UDP (PeersResolver, withUDPNetwork)
import Test.Network.Ports (randomUnusedTCPPorts)
import Test.QuickCheck (resize)
import Text.Show (Show (show))

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

  prop "can send and receive large messages" $ \(msg :: Message) -> do
    [port1, port2] <- randomUnusedTCPPorts 2
    receivedByBob <- atomically $ newEmptyTMVar
    let alice = Host "127.0.0.1" $ fromIntegral port1
        bob = Host "127.0.0.1" $ fromIntegral port2

    showLogsOnFailure $ \tracer ->
      withUDPNetwork tracer alice (resolve [bob]) noop $ \Network{broadcast = aliceSends} ->
        withUDPNetwork tracer bob (resolve [alice]) (atomically . putTMVar receivedByBob) $ \_ -> do
          aliceSends msg
          failAfter 2 $
            atomically (takeTMVar receivedByBob) `shouldReturn` msg

newtype Message = Message {bytes :: ByteString}
  deriving newtype (Eq, ToCBOR, FromCBOR)

instance Show Message where
  show Message{bytes} = "<" <> show (BS.length bytes) <> " random bytes>"

instance ToJSON Message where
  toJSON (Message bytes) =
    String $ (decodeUtf8 $ Hex.encode $ BS.take 16 bytes) <> "..."

instance Arbitrary Message where
  arbitrary = Message . BS.pack <$> resize 10000 arbitrary
  shrink Message{bytes} =
    let len = BS.length bytes
     in if len > 0
          then [Message $ BS.take (len * 9 `div` 10) bytes]
          else []

shouldReceiveAll :: (Ord a) => TVar IO [a] -> [a] -> IO ()
shouldReceiveAll var expected =
  atomically $ do
    received <- sort <$> readTVar var
    check $ received == expected

resolve :: [Host] -> PeersResolver IO a
resolve hosts = const $ pure hosts
