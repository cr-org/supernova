{-# LANGUAGE OverloadedStrings #-}

module Pulsar.Core where

import           Control.Exception              ( throwIO )
import           Control.Monad.Catch            ( MonadThrow )
import           Control.Monad.IO.Class
import qualified Data.Binary                   as B
import           Data.Text                      ( Text )
import           Lens.Family
import           Proto.PulsarApi
import qualified Proto.PulsarApi_Fields        as F
import           Pulsar.Connection
import qualified Pulsar.Protocol.Commands      as P
import           Pulsar.Protocol.Frame          ( Payload(..)
                                                , Response(..)
                                                )
import           Pulsar.Types
import           UnliftIO.Chan

------ Simple commands ------

lookup :: Connection -> Chan Response -> Topic -> IO ()
lookup (Conn s) chan topic = do
  logRequest $ P.lookup topic
  sendSimpleCmd s $ P.lookup topic
  cmd <- getCommand <$> readChan chan
  case cmd ^. F.maybe'lookupTopicResponse of
    Just s  -> logResponse cmd -- TODO: we may need to analyze it and re-issue another lookup
    Nothing -> return ()

newProducer :: Connection -> Chan Response -> B.Word64 -> Topic -> IO Text
newProducer (Conn s) chan pid topic = do
  logRequest $ P.producer pid topic
  sendSimpleCmd s $ P.producer pid topic
  cmd <- getCommand <$> readChan chan
  logResponse cmd
  case cmd ^. F.maybe'producerSuccess of
    Just ps -> return $ ps ^. F.producerName
    Nothing -> return ""

closeProducer :: Connection -> Chan Response -> B.Word64 -> IO ()
closeProducer (Conn s) chan pid = do
  logRequest $ P.closeProducer pid
  sendSimpleCmd s $ P.closeProducer pid
  cmd <- getCommand <$> readChan chan
  case cmd ^. F.maybe'success of
    Just s  -> logResponse cmd
    Nothing -> return () -- TODO: we may need to check for failure too

newSubscriber
  :: Connection
  -> Chan Response
  -> B.Word64
  -> Topic
  -> SubscriptionName
  -> IO ()
newSubscriber (Conn s) chan cid topic subs = do
  logRequest $ P.subscribe cid topic subs
  sendSimpleCmd s $ P.subscribe cid topic subs
  cmd <- getCommand <$> readChan chan
  case cmd ^. F.maybe'success of
    Just s  -> logResponse cmd
    Nothing -> return () -- TODO: we may need to check for failure too

flow :: Connection -> B.Word64 -> IO ()
flow (Conn s) cid = do
  logRequest $ P.flow cid
  sendSimpleCmd s $ P.flow cid

ack :: MonadIO m => Connection -> B.Word64 -> CommandMessage -> m ()
ack (Conn s) cid msg = do
  let msgId = msg ^. F.messageId
  logRequest $ P.ack cid msgId
  sendSimpleCmd s $ P.ack cid msgId

closeConsumer :: Connection -> Chan Response -> B.Word64 -> IO ()
closeConsumer (Conn s) chan cid = do
  logRequest $ P.closeConsumer cid
  sendSimpleCmd s $ P.closeConsumer cid
  cmd <- getCommand <$> readChan chan
  case cmd ^. F.maybe'success of
    Just s  -> logResponse cmd
    Nothing -> return () -- TODO: we may need to check for failure too

------ Keep Alive -------

ping :: (MonadThrow m, MonadIO m) => Connection -> Chan Response -> m ()
ping (Conn s) chan = do
  logRequest P.ping
  sendSimpleCmd s P.ping
  cmd <- getCommand <$> readChan chan
  case cmd ^. F.maybe'pong of
    Just p  -> logResponse p
    Nothing -> liftIO . throwIO $ userError "Failed to get PONG"

pong :: MonadIO m => Connection -> m ()
pong (Conn s) = do
  logRequest P.pong
  sendSimpleCmd s P.pong

------ Payload commands ------

send
  :: Connection
  -> Chan Response
  -> B.Word64
  -> B.Word64
  -> PulsarMessage
  -> IO ()
send (Conn s) chan pid sid (PulsarMessage msg) = do
  logRequest $ P.send pid sid
  sendPayloadCmd s (P.send pid sid) P.messageMetadata (Just $ Payload msg)
  confirmReception
 where
  confirmReception = do
    cmd <- getCommand <$> readChan chan
    case cmd ^. F.maybe'sendReceipt of
      Just s ->
        let seq = s ^. F.sequenceId
        in  if seq == sid then logResponse cmd else confirmReception
      Nothing -> confirmReception
