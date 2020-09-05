{-# LANGUAGE LambdaCase, OverloadedStrings, RankNTypes #-}

{- A simple TCP client, used to communicate with the Pulsar server -}
module Pulsar.Internal.TCPClient
  ( acquireSocket
  )
where

import           Control.Monad.Catch            ( bracket
                                                , bracketOnError
                                                )
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import           Control.Monad.Managed          ( MonadManaged
                                                , managed
                                                , using
                                                )
import qualified Network.Socket                as NS

acquireSocket
  :: (MonadIO m, MonadManaged m) => NS.HostName -> NS.ServiceName -> m NS.Socket
acquireSocket host port = do
  addr <- liftIO resolve
  using $ managed (bracket (acquire addr) release)
 where
  acquire = (putStrLn "[ Establishing connection with Pulsar ]" >>) . open
  release = (putStrLn "[ Closing Pulsar connection ]" >>) . NS.close
  resolve = do
    let hints = NS.defaultHints { NS.addrSocketType = NS.Stream }
    NS.getAddrInfo (Just hints) (Just host) (Just port) >>= \case
      [addr] -> pure addr
      _      -> ioError $ userError "Could not resolve socket address"
  open addr = bracketOnError (NS.openSocket addr) NS.close $ \sock -> do
    NS.connect sock $ NS.addrAddress addr
    return sock
