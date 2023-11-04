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
import Data.Bits ((.&.), (.<<.), (.>>.), (.|.))
import qualified Data.ByteString as BS
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
  = Fragmenting {msgId :: MsgId, size :: Int}
  | Reassembling {msgId :: MsgId, size :: Int}
  | ReceivedFragment {msgId :: MsgId, max :: Word16, current :: Word16}
  | ReceivedAck {msgId :: MsgId, acks :: Word32}
  | Resending Int
  | Acking Int
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
      -- ^ The 12 low order bitst contain the highest fragment number for given message id acknowledged by remote peer.
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
    withAsync (handleFragments tracer fragments broadcast) $ \_ ->
      action (fragment tracer fragments broadcast)

handleFragments ::
  (MonadDelay m, MonadAsync m) =>
  Tracer m (FragmentLog msg) ->
  TVar m Fragments ->
  (Packet ByteString -> m ()) ->
  m ()
handleFragments tracer fragments broadcast =
  concurrently_ ackFragments resendUnackedFragments
 where
  ackFragments = do
    threadDelay 100_000
    Fragments{incoming} <- readTVarIO fragments
    let acks = mapMaybe (uncurry acknowledge) $ Map.toList incoming
    traceWith tracer (Acking (length acks))
    forM_ acks broadcast

  resendUnackedFragments = do
    threadDelay 130_000
    Fragments{outgoing} <- readTVarIO fragments
    let unacked = concatMap (uncurry unackedFragments) $ Map.toList outgoing
    traceWith tracer (Resending (length unacked))
    forM_ unacked broadcast

unackedFragments :: MsgId -> Vector (ByteString, Bool) -> [Packet ByteString]
unackedFragments msgId vec =
  let notAcked = findIndices (not . snd) vec
      len = length vec
      mkFragment i fs = asFragment msgId (fromIntegral len) i (fst $ vec Vector.! i) : fs
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
              pure $ toList $ imap (asFragment msgId (fromIntegral $ length chunks)) $ fmap fst chunks
            traceWith tracer (Fragmenting msgId len)
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
    traceWith tracer (ReceivedFragment msgId maxFragment curFragment)
    msgM <- atomically $ do
      frags@Fragments{incoming} <- readTVar fragments
      let vec' = case Map.lookup msgId incoming of
            Nothing ->
              Vector.modify (\v -> unsafeWrite v (fromIntegral curFragment) (payload, True)) $
                Vector.replicate (fromIntegral maxFragment) ("", False)
            Just vec ->
              Vector.modify (\v -> unsafeWrite v (fromIntegral curFragment) (payload, True)) vec
          isComplete = isNothing $ Vector.findIndex (not . snd) vec'
          msg' =
            if isComplete
              then Just $ foldl' (<>) "" $ fmap fst vec'
              else Nothing
          incoming' =
            if isComplete
              then Map.delete msgId incoming
              else Map.insert msgId vec' incoming
      writeTVar fragments (frags{incoming = incoming'})
      pure msg'
    case msgM of
      Nothing -> pure ()
      Just bytes ->
        case deserialiseFromBytes (fromCBOR @msg) (LBS.fromStrict bytes) of
          Left err -> error $ "TODO: handle error: " <> show err
          Right (_, m) -> do
            traceWith tracer (Reassembling msgId (BS.length bytes))
            callback m
  Ack msgId ack -> do
    atomically $ do
      frags@Fragments{outgoing} <- readTVar fragments
      let outgoing' = case Map.lookup msgId outgoing of
            Nothing -> outgoing
            Just vec ->
              let vec' = updateAckIds vec ack
               in Map.insert msgId vec' outgoing
      writeTVar fragments $ frags{outgoing = outgoing'}
    traceWith tracer (ReceivedAck msgId ack)

updateAckIds :: Vector (ByteString, Bool) -> Word32 -> Vector (ByteString, Bool)
updateAckIds vec ack =
  let acked = {-# SCC compute_ack_to_update #-} computeAckIds ack
   in Vector.modify
        ( \v -> forM_ acked $ \w -> do
            let i = fromIntegral w
            (payload, _) <- v `read` i
            write v i (payload, True)
        )
        vec

acknowledge :: MsgId -> Vector (ByteString, Bool) -> Maybe (Packet ByteString)
acknowledge msgId vec = do
  maxAck <- findIndexR snd vec
  let otherAcks = fmap (\i -> maxAck - i) $ findIndices snd $ Vector.take maxAck vec
      ackBits = foldl' (\bits i -> (1 .<<. (i - 1)) .|. bits) 0 otherAcks
      ack = maxAck .|. (ackBits .<<. 12)
  pure $ Ack msgId (fromIntegral ack)

computeAckIds :: Word32 -> [Word32]
computeAckIds ack =
  let maxAck = ack .&. 0x00000fff
      ackBits = ack .>>. 12
      ackIdxs =
        let bit = 0x1
         in snd $
              foldl'
                ( \(cur, bs) b ->
                    let next = cur .<<. 1
                     in ( next
                        , if ackBits .&. cur > 0
                            then (maxAck - b - 1) : bs
                            else bs
                        )
                )
                (bit, [])
                [0 .. 19]
   in maxAck : ackIdxs
