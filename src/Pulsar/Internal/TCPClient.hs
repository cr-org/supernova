{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Pulsar.Internal.TCPClient
  ( acquireSocket
  )
where

import qualified Control.Exception             as E
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import qualified Network.Socket                as NS

acquireSocket
  :: (MonadIO m, MonadResource m)
  => NS.HostName
  -> NS.ServiceName
  -> m (ReleaseKey, NS.Socket)
acquireSocket host port = do
  addr <- liftIO resolve
  allocate (open addr) NS.close
 where
  resolve = do
    let hints = NS.defaultHints { NS.addrSocketType = NS.Stream }
    NS.getAddrInfo (Just hints) (Just host) (Just port) >>= \case
      [addr] -> pure addr
      _      -> E.ioError $ userError "Could not resolve socket address"
  open addr = E.bracketOnError (NS.openSocket addr) NS.close $ \sock -> do
    NS.connect sock $ NS.addrAddress addr
    return sock
