module Pulsar.Core where

import           Control.Monad.IO.Class
import qualified Data.Binary                   as B
import           Lens.Family
import           Proto.PulsarApi
import qualified Proto.PulsarApi_Fields        as F
import           Pulsar.Connection
import qualified Pulsar.Protocol.Commands      as P
import           Pulsar.Protocol.Frame          ( Payload(..) )
import           Pulsar.Types

logRequest :: (MonadIO m, Show a) => a -> m ()
logRequest cmd = liftIO . putStrLn $ ">>> " <> show cmd

logResponse :: (MonadIO m, Show a) => a -> m ()
logResponse cmd = liftIO . putStrLn $ "<<< " <> show cmd

------ Simple commands ------

ping :: MonadIO m => Connection -> m ()
ping (Conn s) = do
  logRequest P.ping
  sendSimpleCmd s P.ping
  resp <- receive s
  logResponse resp

lookup :: Connection -> Topic -> IO ()
lookup (Conn s) topic = do
  logRequest $ P.lookup topic
  sendSimpleCmd s $ P.lookup topic
  resp <- receive s
  logResponse resp

newProducer :: Connection -> B.Word64 -> Topic -> IO ()
newProducer (Conn s) pid topic = do
  logRequest $ P.producer pid topic
  sendSimpleCmd s $ P.producer pid topic
  resp <- receive s
  logResponse resp

closeProducer :: Connection -> B.Word64 -> IO ()
closeProducer (Conn s) pid = do
  logRequest $ P.closeProducer pid
  sendSimpleCmd s $ P.closeProducer pid
  resp <- receive s
  logResponse resp

newSubscriber :: Connection -> B.Word64 ->Topic -> SubscriptionName -> IO ()
newSubscriber (Conn s) cid topic subs = do
  logRequest $ P.subscribe cid topic subs
  sendSimpleCmd s $ P.subscribe cid topic subs
  resp <- receive s
  logResponse resp

flow :: Connection -> B.Word64 -> IO ()
flow (Conn s) cid = do
  logRequest $ P.flow cid
  sendSimpleCmd s $ P.flow cid

ack :: MonadIO m => Connection -> B.Word64 -> CommandMessage -> m ()
ack (Conn s) cid msg = do
  let msgId = msg ^. F.messageId
  logRequest $ P.ack cid msgId
  sendSimpleCmd s $ P.ack cid msgId

closeConsumer :: Connection -> B.Word64 -> IO ()
closeConsumer (Conn s) cid = do
  logRequest $ P.closeConsumer cid
  sendSimpleCmd s $ P.closeConsumer cid
  resp <- receive s
  logResponse resp

------ Payload commands ------

send :: Connection -> B.Word64 -> PulsarMessage -> IO ()
send (Conn s) pid (PulsarMessage msg) = do
  logRequest $ P.send pid
  sendPayloadCmd s (P.send pid) P.messageMetadata (Just $ Payload msg)
  resp <- receive s
  logResponse resp
