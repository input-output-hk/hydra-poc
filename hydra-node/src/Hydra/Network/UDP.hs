{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wwarn #-}

-- | An experimental network component for Hydra node that uses UDP.
module Hydra.Network.UDP where

import Hydra.Prelude

import Codec.CBOR.Read (deserialiseFromBytes)
import Codec.CBOR.Write (toStrictByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.IP as IP
import Data.Text (unpack)
import Hydra.Logging (Tracer, traceWith)
import Hydra.Network (Host (..), Network (..), NetworkCallback, NetworkComponent)
import Network.Socket (
  SockAddr (..),
  Socket,
  SocketType (Datagram),
  addrAddress,
  addrFamily,
  bind,
  defaultProtocol,
  getAddrInfo,
  socket,
 )
import Network.Socket.ByteString (recvFrom, sendTo)

type PeersResolver m msg = (msg -> m [Host])

data UDPLog msg
  = UDPSent msg Int (Maybe Host)
  | UDPReceived msg (Maybe Host)
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

withUDPNetwork ::
  (FromCBOR msg, ToCBOR msg) =>
  Tracer IO (UDPLog msg) ->
  -- | The address to bind the server on.
  Host ->
  -- | An `IO` action to "resolve" the peers to send a message to.
  PeersResolver IO msg ->
  NetworkComponent IO msg msg a
withUDPNetwork tracer localhost peersResolver callback action = do
  sock <- createSocket localhost
  withBaseUDPNetwork tracer sock peersResolver callback action

withBaseUDPNetwork ::
  (FromCBOR msg, ToCBOR msg) =>
  Tracer IO (UDPLog msg) ->
  Socket ->
  PeersResolver IO msg ->
  NetworkComponent IO msg msg a
withBaseUDPNetwork tracer sock peersResolver callback action =
  withAsync (udpServer tracer sock callback) $ \_ ->
    action (udpClient tracer sock peersResolver)

udpClient ::
  (ToCBOR msg) =>
  Tracer IO (UDPLog msg) ->
  Socket ->
  PeersResolver IO msg ->
  Network IO msg
udpClient tracer sock peersResolver =
  Network $ \msg ->
    let payload = toStrictByteString $ toCBOR msg
     in peersResolver msg >>= traverse_ (sendUDP msg payload)
 where
  sendUDP msg payload Host{hostname, port} = do
    addrinfos <- getAddrInfo Nothing (Just $ unpack hostname) (Just $ show port)
    case addrinfos of
      ((addrAddress -> remoteAddr) : _) -> do
        -- TODO: chunk the payload if too large. In theory, UDP messages can be several
        -- 10s of KBs large but in practice, routers can drop messages larger than ~512 bytes
        -- so it's necessary to chunk and reassemble larger messages.
        void $ sendTo sock payload remoteAddr
        traceWith tracer (UDPSent msg (BS.length payload) (fromSockAddr remoteAddr))
      _ -> error "TODO: what if address is not resolved"

-- | Maximum size of packets to send.
-- This number is a conservative estimation derived from <this article https://gafferongames.com/post/packet_fragmentation_and_reassembly/>
maxPacketSize :: Int
maxPacketSize = 1200

udpServer ::
  forall msg.
  (FromCBOR msg) =>
  Tracer IO (UDPLog msg) ->
  Socket ->
  NetworkCallback msg IO ->
  IO ()
udpServer tracer sock callback = do
  forever receiveMsg
 where
  receiveMsg = do
    -- TODO: reassemble large messages
    (bytes, from) <- recvFrom sock maxPacketSize
    case deserialiseFromBytes (fromCBOR @msg) (LBS.fromStrict bytes) of
      Left err -> error $ "TODO: handle error: " <> show err
      Right (_, msg) -> traceWith tracer (UDPReceived msg (fromSockAddr from)) >> callback msg

fromSockAddr :: SockAddr -> Maybe Host
fromSockAddr addr =
  uncurry Host . first show <$> IP.fromSockAddr addr

createSocket :: Host -> IO Socket
createSocket Host{hostname, port} = do
  addrinfos <- getAddrInfo Nothing (Just $ unpack hostname) (Just $ show port)
  case addrinfos of
    (serveraddr : _) -> do
      sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
      bind sock (addrAddress serveraddr)
      pure sock
    _ -> error "TODO: what if address is not resolved"
