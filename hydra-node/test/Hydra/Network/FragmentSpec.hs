{-# LANGUAGE TypeApplications #-}

module Hydra.Network.FragmentSpec where

import Hydra.Prelude hiding (show)
import Test.Hydra.Prelude

import Control.Concurrent.Class.MonadSTM (
  newTQueueIO,
  newTVarIO,
  readTQueue,
  writeTQueue,
  writeTVar,
 )
import Control.Monad.IOSim (runSimTrace, traceResult)
import Data.Aeson (Value (String))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Hex
import Data.Text (unpack)
import qualified Data.Vector as Vector
import Hydra.Network (Network (..))
import Hydra.Network.Fragment (FragmentLog, Packet (..), acknowledge, updateAckIds, withFragmentHandler)
import Hydra.Network.ReliabilitySpec (noop)
import System.Random (mkStdGen, uniformR)
import Test.QuickCheck (NonEmptyList (..), conjoin, counterexample, property, resize, (===), (==>))
import Test.Util (printTrace, traceInIOSim)
import Text.Show (Show (show))

spec :: Spec
spec = do
  prop "compute and extract acked fragments are inverse" $ \msgId (NonEmpty boolVec) ->
    (length boolVec <= 20)
      ==> let fragmentsVec = Vector.zip (Vector.replicate (length boolVec) "") $ Vector.fromList boolVec
              allFalse = Vector.replicate (length boolVec) ("", False)
              packet = acknowledge msgId fragmentsVec
           in case packet of
                Nothing -> property (not $ and boolVec)
                Just (Ack mid acks) ->
                  let updatedAcked = updateAckIds allFalse acks
                   in conjoin
                        [ fragmentsVec === updatedAcked
                        , mid === msgId
                        ]
                _ ->
                  property False
                    & counterexample ("packet: " <> show packet)
                    & counterexample ("fragments: " <> show fragmentsVec)

  prop "can send and receive large messages" $ \(msg :: Msg) seed ->
    let result =
          runSimTrace $ do
            let tracer = traceInIOSim
            receivedByBob <- newTVarIO Nothing
            aliceToBob <- newTQueueIO
            bobToAlice <- newTQueueIO
            randomness <- newTVarIO $ mkStdGen seed

            let aliceNetwork = network randomness (bobToAlice, aliceToBob)
                bobNetwork = network randomness (aliceToBob, bobToAlice)

            withFragmentHandler tracer aliceNetwork noop $ \Network{broadcast = aliceSends} ->
              withFragmentHandler tracer bobNetwork (atomically . writeTVar receivedByBob . Just) $ \_ -> do
                aliceSends msg
                threadDelay 200_000_000
                atomically $ readTVar receivedByBob
     in ( case traceResult False result of
            Right x -> x === Just msg
            _ -> property False
        )
          & counterexample ("trace:" <> unpack (printTrace (Proxy @(FragmentLog Msg)) result))
 where
  network seed (readQueue, writeQueue) callback action =
    withAsync
      ( forever $ do
          newMsg <- atomically $ readTQueue readQueue
          callback newMsg
      )
      $ \_ ->
        action $
          Network
            { broadcast = \m -> atomically $ do
                -- drop 2% of messages
                r <- randomNumber seed
                unless (r < 0.02) $
                  writeTQueue writeQueue m
            }

  randomNumber seed' = do
    genSeed <- readTVar seed'
    let (res, newGenSeed) = uniformR (0 :: Double, 1) genSeed
    writeTVar seed' newGenSeed
    pure res

newtype Msg = Msg {bytes :: ByteString}
  deriving newtype (Eq, ToCBOR, FromCBOR)

instance Show Msg where
  show Msg{bytes} = "<" <> show (BS.length bytes) <> " random bytes>"

instance ToJSON Msg where
  toJSON (Msg bytes) =
    String $ (decodeUtf8 $ Hex.encode $ BS.take 16 bytes) <> "..."

instance Arbitrary Msg where
  arbitrary = Msg . BS.pack <$> resize 10000 arbitrary
  shrink Msg{bytes} =
    let len = BS.length bytes
     in if len > 0
          then [Msg $ BS.take (len * 9 `div` 10) bytes]
          else []
