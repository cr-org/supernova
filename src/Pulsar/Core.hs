{-# LANGUAGE FlexibleContexts #-}

module Pulsar.Core where

import           Control.Monad.IO.Class
import           Control.Monad.Managed
import           Control.Monad.Reader
import           Data.IORef
import           Pulsar.Connection
import           Pulsar.Internal.Core
import qualified Pulsar.Protocol.Commands      as P
import           Pulsar.Protocol.Frame          ( Metadata(..)
                                                , Payload(..)
                                                )
import           Pulsar.Types

class Monad m => MonadPulsar m where
  liftPulsar :: Pulsar a -> m a

instance MonadPulsar Pulsar where
  liftPulsar = id

runPulsar :: Connection -> Pulsar a -> IO a
runPulsar conn (Pulsar m) = do
  producers <- newIORef []
  runReaderT m (Ctx conn producers)

------ Simple commands ------

ping :: (MonadIO m, MonadReader PulsarCtx m) => m ()
ping = do
  (Ctx (Conn s) _) <- ask
  liftIO . print $ P.ping
  sendSimpleCmd s P.ping
  receive s

newProducer :: (MonadIO m, MonadReader PulsarCtx m) => Topic -> m ()
newProducer topic = do
  (Ctx (Conn s) _) <- ask
  liftIO . print $ P.producer topic
  sendSimpleCmd s $ P.producer topic
  receive s

closeProducer :: (MonadIO m, MonadReader PulsarCtx m) => m ()
closeProducer = do
  (Ctx (Conn s) _) <- ask
  liftIO . print $ P.closeProducer
  sendSimpleCmd s P.closeProducer
  receive s

newSubscriber
  :: (MonadIO m, MonadReader PulsarCtx m) => Topic -> SubscriptionName -> m ()
newSubscriber topic subs = do
  (Ctx (Conn s) _) <- ask
  liftIO . print $ P.subscribe topic subs
  sendSimpleCmd s $ P.subscribe topic subs
  receive s

lookup :: (MonadIO m, MonadReader PulsarCtx m) => Topic -> m ()
lookup topic = do
  (Ctx (Conn s) _) <- ask
  liftIO . print $ P.lookup topic
  sendSimpleCmd s $ P.lookup topic
  receive s

------ Payload commands ------

send :: (MonadIO m, MonadReader PulsarCtx m) => PulsarMessage -> m ()
send (PulsarMessage msg) = do
  (Ctx (Conn s) _) <- ask
  liftIO . print $ P.send
  sendPayloadCmd s P.send (Left P.singleMessageMetadata) (Just $ Payload msg)
  receive s
