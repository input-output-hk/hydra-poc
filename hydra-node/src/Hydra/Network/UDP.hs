{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wwarn #-}

-- | An experimental network component for Hydra node that uses UDP.
module Hydra.Network.UDP where

import Hydra.Prelude

import Codec.CBOR.Read (deserialiseFromBytes)
import Codec.CBOR.Write (toStrictByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Text (unpack)
import Hydra.Logging (Tracer, traceWith)
import Hydra.Network (Host (..), Network (..), NetworkCallback, NetworkComponent)
import Network.Socket (
  Socket,
  SocketType (Datagram),
  addrAddress,
  addrFamily,
  bind,
  defaultProtocol,
  getAddrInfo,
  socket, SockAddr (..),
 )
import Network.Socket.ByteString (recvFrom, sendTo)
import qualified Data.IP as IP

type PeersResolver m msg = (msg -> m [Host])

data UDPLog msg
  = UDPLog
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
  withAsync (udpServer tracer sock callback) $ \_ ->
    action (udpClient sock peersResolver)

udpClient :: (ToCBOR msg) => Socket -> PeersResolver IO msg -> Network IO msg
udpClient sock peersResolver =
  Network $ \msg ->
    let payload = toStrictByteString $ toCBOR msg
     in peersResolver msg >>= traverse_ (sendUDP sock payload)

sendUDP :: Socket -> ByteString -> Host -> IO ()
sendUDP sock payload Host{hostname, port} = do
  addrinfos <- getAddrInfo Nothing (Just $ unpack hostname) (Just $ show port)
  case addrinfos of
    (remoteAddr : _) -> do
      -- TODO: chunk the payload if too large. In theory, UDP messages can be several
      -- 10s of KBs large but in practice, routers can drop messages larger than ~512 bytes
      -- so it's necessary to chunk and reassemble larger messages.
      void $ sendTo sock payload (addrAddress remoteAddr)
    _ -> error "TODO: what if address is not resolved"

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
    (bytes, from) <- recvFrom sock 4096
    case deserialiseFromBytes (fromCBOR @msg) (LBS.fromStrict bytes) of
      Left err -> error $ "TODO: handle error: " <> show err
      Right (_, msg) -> traceWith tracer (UDPReceived msg (fromSockAddr from)) >> callback msg

fromSockAddr :: SockAddr -> Maybe Host
fromSockAddr addr = do
  (ip, port) <- IP.fromSockAddr addr
  pure $ Host (show ip) port

createSocket :: Host -> IO Socket
createSocket Host{hostname, port} = do
  addrinfos <- getAddrInfo Nothing (Just $ unpack hostname) (Just $ show port)
  case addrinfos of
    (serveraddr : _) -> do
      sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
      bind sock (addrAddress serveraddr)
      pure sock
    _ -> error "TODO: what if address is not resolved"
