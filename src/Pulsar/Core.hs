{-# LANGUAGE FlexibleContexts, GeneralizedNewtypeDeriving #-}

module Pulsar.Core where

import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource
import           Pulsar.Commands
import           Pulsar.Connection
import           Pulsar.Data

newtype Pulsar a = Pulsar (ReaderT Connection IO a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Connection)

class Monad m => MonadPulsar m where
  liftPulsar :: Pulsar a -> m a

instance MonadPulsar Pulsar where
  liftPulsar = id

ping :: (MonadIO m, MonadReader Connection m) => m ()
ping = do
  (Conn s) <- ask
  liftIO . print $ cmdPing
  send s cmdPing
  receive s

producer :: (MonadIO m, MonadReader Connection m) => Topic -> m ()
producer topic = do
  (Conn s) <- ask
  liftIO . print $ cmdProducer topic
  send s $ cmdProducer topic
  receive s

subscribe :: (MonadIO m, MonadReader Connection m) => Topic -> SubscriptionName -> m ()
subscribe topic subs = do
  (Conn s) <- ask
  liftIO . print $ cmdSubscribe topic subs
  send s $ cmdSubscribe topic subs
  receive s

--producer :: (MonadIO m, MonadResource m) => Connection -> m ()
--producer (Conn s) = liftIO $ send s cmdProducer >> receive s

runPulsar :: ResourceT IO Connection -> Pulsar a -> IO a
runPulsar resConn (Pulsar m) = runResourceT $ do
  conn <- resConn
  liftIO $ runReaderT m conn
