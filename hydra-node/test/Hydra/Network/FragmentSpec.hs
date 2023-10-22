module Hydra.Network.FragmentSpec where

import Hydra.Prelude hiding (show)
import Test.Hydra.Prelude

import Control.Concurrent.Class.MonadSTM (
  newEmptyTMVar,
  newTQueueIO,
  newTVarIO,
  putTMVar,
  readTQueue,
  takeTMVar,
  writeTQueue,
  writeTVar,
 )
import Control.Monad.IOSim (runSimOrThrow)
import Data.Aeson (Value (String))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Hex
import Hydra.Logging (showLogsOnFailure)
import Hydra.Network (Network (..))
import Hydra.Network.Fragment (withFragmentHandler)
import Hydra.Network.ReliabilitySpec (noop)
import Hydra.Network.UDP (maxPacketSize)
import System.Random (mkStdGen, uniformR)
import Test.QuickCheck (resize, tabulate, (===))
import Text.Show (Show (show))

spec :: Spec
spec =
  prop "can send and receive large messages" $ \(msg :: Message) seed ->
    let result = runSimOrThrow $ do
          receivedByBob <- atomically $ newEmptyTMVar
          aliceToBob <- newTQueueIO
          bobToAlice <- newTQueueIO
          randomness <- newTVarIO $ mkStdGen seed

          let aliceNetwork = network randomness (bobToAlice, aliceToBob)
              bobNetwork = network randomness (aliceToBob, bobToAlice)

          showLogsOnFailure $ \tracer ->
            withFragmentHandler tracer aliceNetwork noop $ \Network{broadcast = aliceSends} ->
              withFragmentHandler tracer bobNetwork (atomically . putTMVar receivedByBob) $ \_ -> do
                aliceSends msg
                failAfter 2 $
                  atomically (takeTMVar receivedByBob)
     in result
          === msg
          & tabulate "Num. Fragments" ["< " <> show ((BS.length (bytes msg) `div` (maxPacketSize * 10) + 1) * 10)]
 where
  network _seed (readQueue, writeQueue) callback action =
    withAsync
      ( forever $ do
          newMsg <- atomically $ readTQueue readQueue
          callback newMsg
      )
      $ \_ ->
        action $
          Network
            { broadcast = \m -> atomically $ do
                -- -- drop 2% of messages
                -- r <- randomNumber seed
                -- unless (r < 0.02) $
                writeTQueue writeQueue m
            }

  _randomNumber seed' = do
    genSeed <- readTVar seed'
    let (res, newGenSeed) = uniformR (0 :: Double, 1) genSeed
    writeTVar seed' newGenSeed
    pure res

newtype Message = Message {bytes :: ByteString}
  deriving newtype (Eq, ToCBOR, FromCBOR)

instance Show Message where
  show Message{bytes} = "<" <> show (BS.length bytes) <> " random bytes>"

instance ToJSON Message where
  toJSON (Message bytes) =
    String $ (decodeUtf8 $ Hex.encode $ BS.take 16 bytes) <> "..."

instance Arbitrary Message where
  arbitrary = Message . BS.pack <$> resize 100000 arbitrary
  shrink Message{bytes} =
    let len = BS.length bytes
     in if len > 0
          then [Message $ BS.take (len * 9 `div` 10) bytes]
          else []
