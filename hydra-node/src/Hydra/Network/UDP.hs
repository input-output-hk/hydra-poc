{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wwarn #-}

-- | An experimental network component for Hydra node that uses UDP.
module Hydra.Network.UDP where

import Hydra.Prelude

import Codec.CBOR.Read (deserialiseFromBytes)
import Codec.CBOR.Write (toStrictByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.IP qualified as IP
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

data UDPMsg msg = UDPMsg {host :: Maybe Host, payload :: msg}
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

data UDPLog msg
  = UDPSent {msg :: UDPMsg msg, size :: Int}
  | UDPReceived {msg :: UDPMsg msg}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

withUDPNetwork ::
  (FromCBOR msg, ToCBOR msg) =>
  Tracer IO (UDPLog msg) ->
  -- | The address to bind the server on.
  Host ->
  NetworkComponent IO (UDPMsg msg) (UDPMsg msg) a
withUDPNetwork tracer localhost callback action = do
  sock <- createSocket localhost
  withBaseUDPNetwork tracer sock callback action

withBaseUDPNetwork ::
  (FromCBOR msg, ToCBOR msg) =>
  Tracer IO (UDPLog msg) ->
  Socket ->
  NetworkComponent IO (UDPMsg msg) (UDPMsg msg) a
withBaseUDPNetwork tracer sock callback action =
  withAsync (udpServer tracer sock callback) $ \_ ->
    action (udpClient tracer sock)

udpClient ::
  ToCBOR msg =>
  Tracer IO (UDPLog msg) ->
  Socket ->
  Network IO (UDPMsg msg)
udpClient tracer sock =
  Network sendUDP
 where
  sendUDP msg@UDPMsg{host, payload} = do
    let bytes = toStrictByteString $ toCBOR payload
    addrinfos <- getAddrInfo Nothing (unpack . hostname <$> host) (show . port <$> host)
    case addrinfos of
      ((addrAddress -> remoteAddr) : _) -> do
        void $ sendTo sock bytes remoteAddr
        traceWith tracer (UDPSent msg (BS.length bytes))
      _ -> error "TODO: what if address is not resolved"

-- | Maximum size of packets to send.
-- This number is a conservative estimation derived from <this article https://gafferongames.com/post/packet_fragmentation_and_reassembly/>
maxPacketSize :: Int
maxPacketSize = 1200

udpServer ::
  forall msg.
  FromCBOR msg =>
  Tracer IO (UDPLog msg) ->
  Socket ->
  NetworkCallback (UDPMsg msg) IO ->
  IO ()
udpServer tracer sock callback = do
  forever receiveMsg
 where
  receiveMsg = do
    (bytes, from) <- recvFrom sock maxPacketSize
    case deserialiseFromBytes (fromCBOR @msg) (LBS.fromStrict bytes) of
      Left err -> error $ "TODO: handle error: " <> show err
      Right (_, msg) ->
        let
          host = fromSockAddr from
          udpMessage = UDPMsg{host, payload = msg}
         in
          traceWith tracer (UDPReceived udpMessage) >> callback udpMessage

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
