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
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as Map
import Data.Vector (Vector, imap)
import qualified Data.Vector as Vector
import Data.Vector.Mutable (unsafeWrite)
import Hydra.Logging (Tracer, traceWith)
import Hydra.Network (Network (..), NetworkCallback, NetworkComponent)
import Hydra.Network.UDP (maxPacketSize)

newtype MsgId = MsgId {unMsgId :: Hash SHA256 ByteString}
  deriving newtype (Eq, Ord, Show, ToJSON, FromJSON)

data FragmentLog msg
  = Fragmenting {msgId :: MsgId, size :: Int}
  | Reassembling {msgId :: MsgId, size :: Int}
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
  (MonadSTM m, ToCBOR msg, FromCBOR msg) =>
  Tracer m (FragmentLog msg) ->
  NetworkComponent m (Packet ByteString) (Packet ByteString) a ->
  NetworkComponent m msg msg a
withFragmentHandler tracer withNetwork callback action = do
  fragments <- newTVarIO (Fragments mempty mempty)
  withNetwork (reassemble tracer fragments callback) $ \Network{broadcast} ->
    action (fragment tracer fragments broadcast)

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
  _ -> pure ()
