{-# LANGUAGE OverloadedStrings #-}

module Pulsar.Connection where

import           Control.Monad.IO.Class
import           Control.Monad.Managed
import qualified Network.Socket                as NS
import qualified Network.Socket.ByteString.Lazy
                                               as BSL
import           Proto.PulsarApi                ( BaseCommand )
import           Pulsar.Internal.TCPClient      ( acquireSocket )
import qualified Pulsar.Protocol.Commands      as P
import           Pulsar.Protocol.Decoder        ( decodeBaseCommand )
import           Pulsar.Protocol.Encoder        ( encodeBaseCommand )
import           Pulsar.Protocol.Frame          ( Metadata
                                                , Payload
                                                , maxFrameSize
                                                )

newtype Connection = Conn NS.Socket

data ConnectData = ConnData
    { connHost :: NS.HostName
    , connPort :: NS.ServiceName
    } deriving Show

defaultConnectData :: ConnectData
defaultConnectData = ConnData { connHost = "127.0.0.1", connPort = "6650" }

connect
  :: (MonadFail m, MonadIO m, MonadManaged m) => ConnectData -> m Connection
connect (ConnData h p) = do
  sock <- acquireSocket h p
  liftIO $ sendSimpleCmd sock P.connect
  response <- receive sock
  case P.isConnected response of
    Just res -> liftIO . putStrLn $ "<<< " <> show res
    Nothing  -> fail "Could not connect"
  return $ Conn sock

sendSimpleCmd :: MonadIO m => NS.Socket -> BaseCommand -> m ()
sendSimpleCmd s cmd =
  liftIO . BSL.sendAll s $ encodeBaseCommand Nothing Nothing cmd

sendPayloadCmd
  :: MonadIO m => NS.Socket -> BaseCommand -> Metadata -> Maybe Payload -> m ()
sendPayloadCmd s cmd meta payload =
  liftIO . BSL.sendAll s $ encodeBaseCommand (Just meta) payload cmd

receive :: MonadIO m => NS.Socket -> m BaseCommand
receive s = liftIO $ do
  msg <- BSL.recv s (fromIntegral maxFrameSize)
  case decodeBaseCommand msg of
    Left  e        -> fail e
    Right (cmd, _) -> return cmd
