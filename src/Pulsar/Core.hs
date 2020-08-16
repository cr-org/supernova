{-# LANGUAGE FlexibleContexts #-}

module Pulsar.Core where

import           Control.Monad.IO.Class
import           Control.Monad.Managed
import           Control.Monad.Reader
import           Data.IORef
import           Proto.PulsarApi
import           Pulsar.Connection
import           Pulsar.Internal.Core
import qualified Pulsar.Protocol.Commands      as P
import           Pulsar.Protocol.Frame          ( Payload(..) )
import           Pulsar.Types

class Monad m => MonadPulsar m where
  liftPulsar :: Pulsar a -> m a

instance MonadPulsar Pulsar where
  liftPulsar = id

runPulsar :: Connection -> Pulsar a -> IO a
runPulsar conn (Pulsar m) = do
  producers <- newIORef []
  runReaderT m (Ctx conn producers)

logRequest :: (MonadIO m, Show a) => a -> m ()
logRequest cmd = liftIO . putStrLn $ ">>> " <> show cmd

logResponse :: (MonadIO m, Show a) => a -> m ()
logResponse cmd = liftIO . putStrLn $ "<<< " <> show cmd

------ Simple commands ------

ping :: (MonadIO m, MonadReader PulsarCtx m) => m ()
ping = do
  (Ctx (Conn s) _) <- ask
  logRequest P.ping
  sendSimpleCmd s P.ping
  resp <- receive s
  logResponse resp

lookup :: (MonadIO m, MonadReader PulsarCtx m) => Topic -> m ()
lookup topic = do
  (Ctx (Conn s) _) <- ask
  logRequest $ P.lookup topic
  sendSimpleCmd s $ P.lookup topic
  resp <- receive s
  logResponse resp

newProducer :: (MonadIO m, MonadReader PulsarCtx m) => Topic -> m ()
newProducer topic = do
  (Ctx (Conn s) _) <- ask
  logRequest $ P.producer topic
  sendSimpleCmd s $ P.producer topic
  resp <- receive s
  logResponse resp

closeProducer :: (MonadIO m, MonadReader PulsarCtx m) => m ()
closeProducer = do
  (Ctx (Conn s) _) <- ask
  logRequest P.closeProducer
  sendSimpleCmd s P.closeProducer
  resp <- receive s
  logResponse resp

newSubscriber
  :: (MonadIO m, MonadReader PulsarCtx m) => Topic -> SubscriptionName -> m ()
newSubscriber topic subs = do
  (Ctx (Conn s) _) <- ask
  logRequest $ P.subscribe topic subs
  sendSimpleCmd s $ P.subscribe topic subs
  resp <- receive s
  logResponse resp

flow :: (MonadIO m, MonadReader PulsarCtx m) => m (Maybe MessageIdData)
flow = do
  (Ctx (Conn s) _) <- ask
  logRequest P.flow
  sendSimpleCmd s P.flow
  resp <- receive s
  logResponse resp
  return $ P.getMessageId (getCommand resp)

ack :: (MonadIO m, MonadReader PulsarCtx m) => MessageIdData -> m ()
ack msgId = do
  (Ctx (Conn s) _) <- ask
  logRequest $ P.ack msgId
  sendSimpleCmd s $ P.ack msgId
  resp <- receive s
  logResponse resp

closeConsumer :: (MonadIO m, MonadReader PulsarCtx m) => m ()
closeConsumer = do
  (Ctx (Conn s) _) <- ask
  logRequest P.closeConsumer
  sendSimpleCmd s P.closeConsumer
  resp <- receive s
  logResponse resp

------ Payload commands ------

send :: (MonadIO m, MonadReader PulsarCtx m) => PulsarMessage -> m ()
send (PulsarMessage msg) = do
  (Ctx (Conn s) _) <- ask
  logRequest P.send
  sendPayloadCmd s P.send P.messageMetadata (Just $ Payload msg)
  resp <- receive s
  logResponse resp
