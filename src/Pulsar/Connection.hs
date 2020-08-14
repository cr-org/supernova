{-# LANGUAGE OverloadedStrings #-}

module Pulsar.Connection where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import qualified Network.Socket                as NS
import qualified Network.Socket.ByteString     as BS
import           Proto.PulsarApi                ( BaseCommand )
import           Pulsar.Commands                ( cmdConnect )
import           Pulsar.Internal.Frame          ( encodeBaseCommand )
import           Pulsar.Internal.TCPClient      ( acquireSocket )

newtype Connection = Conn NS.Socket

data ConnectData = ConnData
    { connHost :: NS.HostName
    , connPort :: NS.ServiceName
    } deriving Show

defaultConnectData :: ConnectData
defaultConnectData = ConnData { connHost = "127.0.0.1", connPort = "6650" }

connect :: (MonadIO m, MonadResource m) => ConnectData -> m Connection
connect (ConnData h p) = do
  (_, sock) <- acquireSocket h p
  liftIO $ send sock cmdConnect
  liftIO $ putStrLn "<< Successfully connected to Apache Pulsar >>"
  return $ Conn sock

send :: MonadIO m => NS.Socket -> BaseCommand -> m ()
send s cmd = liftIO . BS.sendAll s $ encodeBaseCommand cmd

--TODO: once we have the parser, this should return IO BaseCommand
receive :: MonadIO m => NS.Socket -> m ()
receive s = liftIO $ do
  msg <- BS.recv s 4096
  print $ "Received: " <> msg
