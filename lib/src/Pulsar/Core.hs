{-# LANGUAGE DataKinds, FlexibleContexts, LambdaCase, OverloadedStrings #-}

{- Defines a set of transactional commands, communicating via internal channels -}
module Pulsar.Core where

import           Control.Concurrent.Chan
import           Control.Exception              ( throwIO )
import           Control.Monad.Catch            ( MonadThrow )
import           Control.Monad.IO.Class
import qualified Data.Binary                   as B
import           Data.Functor                   ( void )
import           Data.ProtoLens.Field           ( HasField )
import           Data.Text                      ( Text )
import           Lens.Family
import           Proto.PulsarApi
import qualified Proto.PulsarApi_Fields        as F
import           Pulsar.Connection
import           Pulsar.Internal.Logger
import qualified Pulsar.Protocol.Commands      as P
import           Pulsar.Protocol.Frame          ( Payload(..)
                                                , Response(..)
                                                , getCommand
                                                )
import           Pulsar.Types

------ Simple commands ------

verifyResponse
  :: (HasField a "requestId" B.Word64, Show a)
  => ReqId
  -> Chan Response
  -> LensLike' (Constant (Maybe a)) BaseCommand (Maybe a)
  -> IO (Maybe a)
verifyResponse r@(ReqId req) chan lens = do
  resp <- readChan chan
  let cmd'    = getCommand resp ^. lens
      req'    = view F.requestId <$> cmd'
      rewrite = writeChan chan resp
      loop    = verifyResponse r chan lens
      checkEq (_, rq) | rq == req = cmd' <$ logResponse resp
                      | otherwise = rewrite >> loop
  maybe loop checkEq $ (,) <$> cmd' <*> req'

lookup :: Connection -> Chan Response -> ReqId -> Topic -> IO ()
lookup (Conn s) chan r@(ReqId req) topic = do
  logRequest $ P.lookup req topic
  sendSimpleCmd s $ P.lookup req topic
  -- TODO: we need to analyze it and might need to re-issue another lookup
  void $ verifyResponse r chan F.maybe'lookupTopicResponse

newProducer
  :: Connection -> Chan Response -> ReqId -> ProducerId -> Topic -> IO Text
newProducer (Conn s) chan r@(ReqId req) (PId pid) topic = do
  logRequest $ P.producer req pid topic
  sendSimpleCmd s $ P.producer req pid topic
  verifyResponse r chan F.maybe'producerSuccess >>= \case
    Just ps -> return $ ps ^. F.producerName
    Nothing -> return ""

closeProducer :: Connection -> Chan Response -> ProducerId -> ReqId -> IO ()
closeProducer (Conn s) chan (PId pid) r@(ReqId req) = do
  logRequest $ P.closeProducer req pid
  sendSimpleCmd s $ P.closeProducer req pid
  void $ verifyResponse r chan F.maybe'success

newSubscriber
  :: Connection
  -> Chan Response
  -> ReqId
  -> ConsumerId
  -> Topic
  -> SubscriptionName
  -> IO ()
newSubscriber (Conn s) chan r@(ReqId req) (CId cid) topic subs = do
  logRequest $ P.subscribe req cid topic subs
  sendSimpleCmd s $ P.subscribe req cid topic subs
  -- TODO: we may need to check for failure too
  void $ verifyResponse r chan F.maybe'success

flow :: Connection -> ConsumerId -> Permits -> IO ()
flow (Conn s) (CId cid) (Permits p) = do
  logRequest $ P.flow cid p
  sendSimpleCmd s $ P.flow cid p

ack :: MonadIO m => Connection -> ConsumerId -> MessageIdData -> m ()
ack (Conn s) (CId cid) msgId = do
  logRequest $ P.ack cid msgId
  sendSimpleCmd s $ P.ack cid msgId

closeConsumer :: Connection -> Chan Response -> ConsumerId -> ReqId -> IO ()
closeConsumer (Conn s) chan (CId cid) r@(ReqId req) = do
  logRequest $ P.closeConsumer req cid
  sendSimpleCmd s $ P.closeConsumer req cid
  void $ verifyResponse r chan F.maybe'success

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
  -> Chan Response
  -> ProducerId
  -> SeqId
  -> PulsarMessage
  -> IO ()
send (Conn s) chan (PId pid) (SeqId sid) (PulsarMessage msg) = do
  logRequest $ P.send pid sid
  sendPayloadCmd s (P.send pid sid) P.messageMetadata (Just $ Payload msg)
  confirmReception
 where
  confirmReception = do
    resp <- readChan chan
    let cmd'    = getCommand resp ^. F.maybe'sendReceipt
        pid'    = view F.producerId <$> cmd'
        sid'    = view F.sequenceId <$> cmd'
        rewrite = writeChan chan resp
        loop    = confirmReception
        checkEq (_, pd, sd) | pd == pid && sd == sid = logResponse resp
                            | otherwise              = rewrite >> loop
    maybe loop checkEq $ (,,) <$> cmd' <*> pid' <*> sid'
