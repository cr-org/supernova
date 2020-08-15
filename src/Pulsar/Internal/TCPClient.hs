{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Pulsar.Internal.TCPClient
  ( acquireSocket
  )
where

import qualified Control.Exception             as E
import           Control.Monad.IO.Class
import           Control.Monad.Managed
import qualified Network.Socket                as NS

acquireSocket
  :: (MonadIO m, MonadManaged m) => NS.HostName -> NS.ServiceName -> m NS.Socket
acquireSocket host port = do
  addr <- liftIO resolve
  using $ managed
    (E.bracket
      (putStrLn "[ Establishing connection with Pulsar ]" >> open addr)
      (\s -> putStrLn "[ Closing Pulsar connection ]" >> NS.close s)
    )
 where
  resolve = do
    let hints = NS.defaultHints { NS.addrSocketType = NS.Stream }
    NS.getAddrInfo (Just hints) (Just host) (Just port) >>= \case
      [addr] -> pure addr
      _      -> E.ioError $ userError "Could not resolve socket address"
  open addr = E.bracketOnError (NS.openSocket addr) NS.close $ \sock -> do
    NS.connect sock $ NS.addrAddress addr
    return sock
