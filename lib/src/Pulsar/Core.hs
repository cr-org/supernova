{-# LANGUAGE OverloadedStrings #-}

{- Defines a set of transactional commands, communicating via internal channels -}
module Pulsar.Core where

import           Control.Concurrent.Chan
import           Control.Concurrent.MVar
import           Control.Exception              ( throwIO )
import           Control.Monad.Catch            ( MonadThrow )
import           Control.Monad.IO.Class
import           Data.Text                      ( Text )
import           Lens.Family
import           Proto.PulsarApi         hiding ( Subscription )
import qualified Proto.PulsarApi_Fields        as F
import           Pulsar.AppState
import           Pulsar.Connection
import           Pulsar.Internal.Logger
import qualified Pulsar.Protocol.Commands      as P
import           Pulsar.Protocol.Frame          ( Payload(..)
                                                , Response(..)
                                                , getCommand
                                                )
import           Pulsar.Types

------ Simple commands ------

lookup :: Connection -> MVar Response -> ReqId -> Topic -> IO ()
lookup (Conn s) var (ReqId req) topic = do
  logRequest $ P.lookup req topic
  sendSimpleCmd s $ P.lookup req topic
  -- TODO: we need to analyze it and might need to re-issue another lookup
  readMVar var >>= logResponse

newProducer
  :: Connection -> MVar Response -> ReqId -> ProducerId -> Topic -> IO Text
newProducer (Conn s) var (ReqId req) (PId pid) topic = do
  logRequest $ P.producer req pid topic
  sendSimpleCmd s $ P.producer req pid topic
  resp <- readMVar var
  logResponse resp
  case getCommand resp ^. F.maybe'producerSuccess of
    Just ps -> return $ ps ^. F.producerName
    Nothing -> return ""

closeProducer :: Connection -> MVar Response -> ProducerId -> ReqId -> IO ()
closeProducer (Conn s) var (PId pid) (ReqId req) = do
  logRequest $ P.closeProducer req pid
  sendSimpleCmd s $ P.closeProducer req pid
  readMVar var >>= logResponse

newSubscriber
  :: Connection
  -> MVar Response
  -> ReqId
  -> ConsumerId
  -> Topic
  -> Subscription
  -> IO ()
newSubscriber (Conn s) var (ReqId req) (CId cid) topic (Subscription stype sname)
  = do
    logRequest $ P.subscribe req cid topic stype sname
    sendSimpleCmd s $ P.subscribe req cid topic stype sname
    readMVar var >>= logResponse

flow :: Connection -> ConsumerId -> Permits -> IO ()
flow (Conn s) (CId cid) (Permits p) = do
  logRequest $ P.flow cid p
  sendSimpleCmd s $ P.flow cid p

ack :: MonadIO m => Connection -> ConsumerId -> MessageIdData -> m ()
ack (Conn s) (CId cid) msgId = do
  logRequest $ P.ack cid msgId
  sendSimpleCmd s $ P.ack cid msgId

closeConsumer :: Connection -> MVar Response -> ConsumerId -> ReqId -> IO ()
closeConsumer (Conn s) var (CId cid) (ReqId req) = do
  logRequest $ P.closeConsumer req cid
  sendSimpleCmd s $ P.closeConsumer req cid
  readMVar var >>= logResponse

------ Keep Alive -------

ping :: (MonadThrow m, MonadIO m) => Connection -> Chan Response -> m ()
ping (Conn s) chan = do
  logRequest P.ping
  sendSimpleCmd s P.ping
  cmd <- getCommand <$> liftIO (readChan chan)
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
  -> MVar Response
  -> ProducerId
  -> SeqId
  -> PulsarMessage
  -> IO ()
send (Conn s) var (PId pid) (SeqId sid) (PulsarMessage msg) = do
  logRequest $ P.send pid sid
  sendPayloadCmd s (P.send pid sid) P.messageMetadata (Just $ Payload msg)
  readMVar var >>= logResponse
