{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}

-- | Handles fragmentation of large messages over an underlying (small)
--  packet transport, eg. UDP.
module Hydra.Network.Fragment where

import Hydra.Prelude

import Cardano.Binary (serialize', unsafeDeserialize')
import Cardano.Crypto.Hash (Hash, SHA256, hashWith)
import Codec.CBOR.Read (deserialiseFromBytes)
import Control.Concurrent.Class.MonadSTM (MonadSTM (..), newTVarIO)
import Data.Bits (testBit, (.&.), (.<<.), (.>>.), (.|.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Hex
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as Map
import Data.Vector (Vector, findIndices, imap)
import qualified Data.Vector as Vector
import Data.Vector.Generic (findIndexR)
import Data.Vector.Mutable (read, unsafeWrite, write)
import Hydra.Logging (Tracer, traceWith)
import Hydra.Network (Network (..), NetworkCallback, NetworkComponent)
import Hydra.Network.UDP (maxPacketSize)
import Test.QuickCheck (vectorOf)

newtype MsgId = MsgId {unMsgId :: Hash SHA256 ByteString}
  deriving newtype (Eq, Ord, Show, ToJSON, FromJSON)

instance Arbitrary MsgId where
  arbitrary = MsgId . hashWith id . BS.pack <$> vectorOf 32 arbitrary

data FragmentLog msg
  = Fragmenting {msgId :: MsgId, size :: Int, numFragments :: Int}
  | Reassembling {msgId :: MsgId, size :: Int}
  | ReceivedFragment {msgId :: MsgId, max :: Word16, current :: Word16}
  | ReceivedAck {msgId :: MsgId, acks :: [Word32]}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Packet msg
  = -- | Fragments of messages that do not fit in a single packet.
    Fragment
      { msgId :: MsgId
      -- ^ Unique id for the whole message
      , maxFragment :: Word16
      -- ^ Maximum fragment number for this message id.
      -- While this is a 16-bits value, fragment numbers are actually defined on
      -- 12 bits, which implies one can send messages at most `4096 x maxPacketSize` long, or
      -- or ~4MB.
      , curFragment :: Word16
      -- ^ Current fragment number, on 12 bits.
      , payload :: ByteString
      -- ^ Payload for this fragment.
      -- By definition, it's either
      --
      --  * Exactly `maxPacketSize` long iff `curFragment < maxFragment`
      --  * Lower than or equal to `maxPacketSize` if `curFragment == maxFragment`
      }
  | -- | A message that fits in a single packet.
    Message
      { msgId :: MsgId
      , message :: msg
      }
  | -- | An acknowledgement message.
    -- Acknowledgment is only relevant for message fragments, never for individual
    -- messages.
    Ack
      { msgId :: MsgId
      -- ^ The id of the message fragment being acknowledged
      , ack :: Word32
      -- ^ The 12 low order bits contain the highest fragment number for given message id acknowledged by remote peer.
      -- the other 20 bits is a bitfield of acknowledged packet ids lower than `ack`.
      -- Bit `n + 12` of this word is set if `ack - n` packet has been received.
      }
  deriving stock (Eq, Show)

withFragmentHandler ::
  (ToCBOR msg, FromCBOR msg, MonadAsync m, MonadDelay m) =>
  Tracer m (FragmentLog msg) ->
  NetworkComponent m (Packet ByteString) (Packet ByteString) a ->
  NetworkComponent m msg msg a
withFragmentHandler tracer withNetwork callback action = do
  fragments <- newTVarIO (Fragments mempty mempty)
  withNetwork (reassemble tracer fragments callback) $ \Network{broadcast} ->
    -- FIXME: Assumes broadcast is threadsafe
    withAsync (handleFragments fragments broadcast) $ \_ ->
      action (fragment tracer fragments broadcast)

handleFragments ::
  (MonadDelay m, MonadAsync m) =>
  TVar m Fragments ->
  (Packet ByteString -> m ()) ->
  m ()
handleFragments fragments broadcast =
  concurrently_ ackFragments resendUnackedFragments
 where
  ackFragments = forever $ do
    threadDelay 100_000
    Fragments{incoming} <- readTVarIO fragments
    let acks = mapMaybe (uncurry acknowledge) $ Map.toList incoming
    forM_ acks broadcast

  resendUnackedFragments = forever $ do
    threadDelay 130_000
    Fragments{outgoing} <- readTVarIO fragments
    let unacked = concatMap (uncurry unackedFragments) $ Map.toList outgoing
    forM_ unacked broadcast

unackedFragments :: MsgId -> Vector (ByteString, Bool) -> [Packet ByteString]
unackedFragments msgId vec =
  let notAcked = findIndices (not . snd) vec
      len = length vec
      mkFragment i fs = asFragment msgId (fromIntegral len - 1) i (fst $ vec Vector.! i) : fs
   in foldr mkFragment [] $ toList notAcked

data Fragments = Fragments
  { outgoing :: Map MsgId (Vector (ByteString, Bool))
  , incoming :: Map MsgId (Vector (ByteString, Bool))
  }

fragment ::
  (ToCBOR msg, MonadSTM m) =>
  Tracer m (FragmentLog msg) ->
  TVar m Fragments ->
  (Packet ByteString -> m ()) ->
  Network m msg
fragment tracer fragments bcast =
  Network broadcast
 where
  broadcast msg =
    let bytes = serialize' msg
        msgId = MsgId $ hashWith id bytes
        len = BS.length bytes
     in if len < maxPacketSize
          then bcast (Message msgId bytes)
          else do
            msgs <- atomically $ do
              frags@Fragments{outgoing} <- readTVar fragments
              let chunks = (,False) <$> split bytes
                  frags' = frags{outgoing = Map.insert msgId chunks outgoing}
              writeTVar fragments frags'
              pure $ toList $ imap (asFragment msgId (fromIntegral (length chunks) - 1)) $ fmap fst chunks
            traceWith tracer (Fragmenting msgId len (length msgs))
            forM_ msgs bcast

asFragment :: MsgId -> Word16 -> Int -> ByteString -> Packet ByteString
asFragment msgId maxFragment idx payload =
  Fragment{msgId, maxFragment, curFragment = fromIntegral idx, payload}

split :: ByteString -> Vector ByteString
split = fromList . go
 where
  go bytes =
    let (slice, rest) = BS.splitAt maxPacketSize bytes
     in slice : if BS.null rest then [] else go rest

reassemble ::
  forall msg m.
  (FromCBOR msg, MonadSTM m) =>
  Tracer m (FragmentLog msg) ->
  TVar m Fragments ->
  NetworkCallback msg m ->
  NetworkCallback (Packet ByteString) m
reassemble tracer fragments callback = \case
  Message _ payload ->
    callback (unsafeDeserialize' payload)
  Fragment{msgId, maxFragment, curFragment, payload} -> do
    msgM <- atomically $ do
      frags@Fragments{incoming} <- readTVar fragments
      let vec' = case Map.lookup msgId incoming of
            Nothing ->
              Vector.modify (\v -> unsafeWrite v (fromIntegral curFragment) (payload, True)) $
                Vector.replicate (fromIntegral $ maxFragment + 1) ("", False)
            Just vec ->
              Vector.modify (\v -> unsafeWrite v (fromIntegral curFragment) (payload, True)) vec
          isComplete = isNothing $ Vector.findIndex (not . snd) vec'
          (msg', incoming') =
            if isComplete
              then (Just $ foldl' (<>) "" $ fmap fst vec', Map.delete msgId incoming)
              else (Nothing, Map.insert msgId vec' incoming)
      writeTVar fragments (frags{incoming = incoming'})
      pure msg'
    case msgM of
      Nothing ->
        traceWith tracer (ReceivedFragment msgId maxFragment curFragment)
      Just bytes ->
        case deserialiseFromBytes (fromCBOR @msg) (LBS.fromStrict bytes) of
          Left err -> error $ "TODO: handle error: " <> show err <> ", msg: " <> show (Hex.encode bytes)
          Right (_, m) -> do
            traceWith tracer (Reassembling msgId (BS.length bytes))
            callback m
  Ack msgId ack -> do
    acked <- atomically $ do
      frags@Fragments{outgoing} <- readTVar fragments
      let (outgoing', acked) = case Map.lookup msgId outgoing of
            Nothing -> (outgoing, []) -- FIXME: this should be an error?
            Just vec ->
              let (vec', acked) = updateAckIds vec ack
               in (Map.insert msgId vec' outgoing, acked)
      writeTVar fragments $ frags{outgoing = outgoing'}
      pure acked
    traceWith tracer (ReceivedAck msgId acked)

updateAckIds :: Vector (ByteString, Bool) -> Word32 -> (Vector (ByteString, Bool), [Word32])
updateAckIds vec ack =
  let acked = computeAckIds ack
      vec' =
        Vector.modify
          ( \v -> forM_ acked $ \w -> do
              let i = fromIntegral w
              (payload, _) <- v `read` i
              write v i (payload, True)
          )
          vec
   in (vec', acked)

acknowledge :: MsgId -> Vector (ByteString, Bool) -> Maybe (Packet ByteString)
acknowledge msgId vec = do
  maxAck <- findIndexR snd vec
  let otherAcks = fmap (\i -> maxAck - i) $ findIndices snd $ Vector.take maxAck vec
      ackBits = foldl' (\bits i -> (1 .<<. (i - 1)) .|. bits) 0 otherAcks
      ack = maxAck .|. (ackBits .<<. 12)
  pure $ Ack msgId (fromIntegral ack)

computeAckIds :: Word32 -> [Word32]
computeAckIds ack =
  let !maxAck = ack .&. 0x00000fff
      !ackBits = ack .>>. 12
      !offset = maxAck - 1
      ackIdxs =
        foldl'
          ( \bs b ->
              if testBit ackBits b
                then let !b' = (offset - fromIntegral b) in b' : bs
                else bs
          )
          []
          [0 .. 19]
   in maxAck : ackIdxs
{-# INLINE computeAckIds #-}
