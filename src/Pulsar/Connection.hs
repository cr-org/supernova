{-# LANGUAGE OverloadedStrings #-}

module Pulsar.Connection where

import           Control.Monad.IO.Class
import           Control.Monad.Managed
import qualified Network.Socket                as NS
import qualified Network.Socket.ByteString.Lazy
                                               as SBL
import           Proto.PulsarApi                ( BaseCommand
                                                , MessageMetadata
                                                )
import           Pulsar.Internal.TCPClient      ( acquireSocket )
import qualified Pulsar.Protocol.Commands      as P
import           Pulsar.Protocol.Decoder        ( decodeBaseCommand )
import           Pulsar.Protocol.Encoder        ( encodeBaseCommand )
import           Pulsar.Protocol.Frame          ( Payload
                                                , Response(..)
                                                , frameMaxSize
                                                )

newtype Connection = Conn NS.Socket

data ConnectData = ConnData
    { connHost :: NS.HostName
    , connPort :: NS.ServiceName
    } deriving Show

defaultConnectData :: ConnectData
defaultConnectData = ConnData { connHost = "127.0.0.1", connPort = "6650" }

-- Could be a lens but not worth it for now
getCommand :: Response -> BaseCommand
getCommand response = case response of
  (SimpleResponse cmd     ) -> cmd
  (PayloadResponse cmd _ _) -> cmd

connect
  :: (MonadFail m, MonadIO m, MonadManaged m) => ConnectData -> m Connection
connect (ConnData h p) = do
  sock <- acquireSocket h p
  liftIO $ sendSimpleCmd sock P.connect
  resp <- receive sock
  case P.getConnected (getCommand resp) of
    Just res -> liftIO . putStrLn $ "<<< " <> show res
    Nothing  -> fail "Could not connect"
  return $ Conn sock

sendSimpleCmd :: MonadIO m => NS.Socket -> BaseCommand -> m ()
sendSimpleCmd s cmd =
  liftIO . SBL.sendAll s $ encodeBaseCommand Nothing Nothing cmd

sendPayloadCmd
  :: MonadIO m => NS.Socket -> BaseCommand -> MessageMetadata -> Maybe Payload -> m ()
sendPayloadCmd s cmd meta payload =
  liftIO . SBL.sendAll s $ encodeBaseCommand (Just meta) payload cmd

receive :: MonadIO m => NS.Socket -> m Response
receive s = liftIO $ do
  msg <- SBL.recv s (fromIntegral frameMaxSize)
  case decodeBaseCommand msg of
    Left  e    -> fail $ "Decoding error: " <> e
    Right resp -> return resp
