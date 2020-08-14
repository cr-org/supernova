{-# LANGUAGE OverloadedStrings #-}

module Pulsar.Connection where

import           Control.Monad.IO.Class
import           Control.Monad.Managed
import qualified Network.Socket                as NS
import qualified Network.Socket.ByteString     as BS
import           Proto.PulsarApi                ( BaseCommand )
import qualified Pulsar.Commands               as P
import           Pulsar.Internal.Frame          ( Metadata
                                                , Payload
                                                , encodeBaseCommand
                                                )
import           Pulsar.Internal.TCPClient      ( acquireSocket )

newtype Connection = Conn NS.Socket

data ConnectData = ConnData
    { connHost :: NS.HostName
    , connPort :: NS.ServiceName
    } deriving Show

defaultConnectData :: ConnectData
defaultConnectData = ConnData { connHost = "127.0.0.1", connPort = "6650" }

connect :: (MonadIO m, MonadManaged m) => ConnectData -> m Connection
connect (ConnData h p) = do
  sock <- acquireSocket h p
  liftIO $ sendSimpleCmd sock P.connect
  liftIO $ putStrLn "<< Successfully connected to Apache Pulsar >>"
  return $ Conn sock

sendSimpleCmd :: MonadIO m => NS.Socket -> BaseCommand -> m ()
sendSimpleCmd s cmd =
  liftIO . BS.sendAll s $ encodeBaseCommand Nothing Nothing cmd

sendPayloadCmd
  :: MonadIO m => NS.Socket -> BaseCommand -> Metadata -> Maybe Payload -> m ()
sendPayloadCmd s cmd meta payload =
  liftIO . BS.sendAll s $ encodeBaseCommand (Just meta) payload cmd

--TODO: once we have the parser, this should return IO BaseCommand
receive :: MonadIO m => NS.Socket -> m ()
receive s = liftIO $ do
  msg <- BS.recv s 4096
  print $ "Received: " <> msg
