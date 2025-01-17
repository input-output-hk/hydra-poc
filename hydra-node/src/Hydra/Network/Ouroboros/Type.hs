module Hydra.Network.Ouroboros.Type where

import Hydra.Prelude

import Cardano.Binary qualified as CBOR
import Codec.CBOR.Read qualified as CBOR
import Network.TypedProtocol.Codec
import Network.TypedProtocol.Codec.CBOR (mkCodecCborLazyBS)
import Network.TypedProtocol.Core
import Ouroboros.Consensus.Util (ShowProxy (..))

-- | TODO explain Protocol
--
-- It is used both as a type level tag for the protocol and as the kind of the
-- types of the states in the protocol state machine. That is @FireForget@ is a
-- kind, and @StIdle@ is a type of that kind.
data FireForget msg where
  StIdle :: FireForget msg
  StDone :: FireForget msg

data SFireForget (st :: FireForget msg) where
  SingIdle :: SFireForget StIdle
  SingDone :: SFireForget StDone

deriving instance Show (SFireForget st)

instance StateTokenI StIdle where
  stateToken = SingIdle
instance StateTokenI StDone where
  stateToken = SingDone

instance ShowProxy (FireForget msg) where
  showProxy _ = "FireForget"

instance Protocol (FireForget msg) where
  -- The actual messages in our protocol.
  --
  -- Messages define the possible transitions between the protocol's state as defined
  -- by `FireForget`. In this particular case things are extremely simple: The protocol
  -- handles `Msg` containing some payload until terminated by `MsgDone`.
  data Message (FireForget msg) from to where
    MsgSend :: msg -> Message (FireForget msg) 'StIdle 'StIdle
    MsgDone :: Message (FireForget msg) 'StIdle 'StDone

  type StateAgency StIdle = ClientAgency
  type StateAgency StDone = NobodyAgency

  type StateToken = SFireForget

deriving stock instance Show msg => Show (Message (FireForget msg) from to)

deriving stock instance Eq msg => Eq (Message (FireForget msg) from to)

codecFireForget ::
  forall m msg.
  (MonadST m, FromCBOR msg, ToCBOR msg) =>
  Codec (FireForget msg) CBOR.DeserialiseFailure m LByteString
codecFireForget =
  mkCodecCborLazyBS encode decode
 where
  encode ::
    forall msg' (st :: FireForget msg') (st' :: FireForget msg').
    ToCBOR msg' =>
    Message (FireForget msg') st st' ->
    CBOR.Encoding
  encode MsgDone = CBOR.encodeWord 0
  encode (MsgSend msg) = CBOR.encodeWord 1 <> toCBOR msg

  decode ::
    forall msg' s (st :: FireForget msg').
    (FromCBOR msg', ActiveState st) =>
    StateToken st ->
    CBOR.Decoder s (SomeMessage st)
  decode stok = do
    key <- CBOR.decodeWord
    case (stok, key) of
      (SingIdle, 0) -> pure $ SomeMessage MsgDone
      (SingIdle, 1) -> SomeMessage . MsgSend <$> fromCBOR
      (_, _) -> fail "codedFireForget.StIdle: unexpected"
