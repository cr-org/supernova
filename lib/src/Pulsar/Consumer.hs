{-# LANGUAGE FlexibleContexts, LambdaCase, OverloadedStrings #-}

module Pulsar.Consumer where

import           Control.Concurrent             ( forkIO
                                                , killThread
                                                )
import           Control.Concurrent.Chan
import           Control.Monad                  ( forever )
import           Control.Monad.Catch            ( bracket )
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import           Control.Monad.Managed          ( managed )
import           Control.Monad.Reader           ( MonadReader
                                                , ask
                                                )
import           Data.Functor                   ( void )
import           Lens.Family
import qualified Proto.PulsarApi_Fields        as F
import qualified Pulsar.Core                   as C
import           Pulsar.Connection
import           Pulsar.Internal.Logger         ( logResponse )
import           Pulsar.Protocol.Frame          ( Payload(..)
                                                , Response(..)
                                                )
import           Pulsar.Types

{- | An abstract 'Consumer' able to 'fetch' messages and 'ack'nowledge them. -}
data Consumer m = Consumer
  { fetch :: m Message   -- ^ Fetches a single message. Blocks if no messages are available.
  , ack :: MsgId -> m () -- ^ Acknowledges a single message.
  }

{- | Create a new 'Consumer' by supplying a 'PulsarCtx' (returned by 'Pulsar.connect'), a 'Topic' and a 'SubscriptionName'. -}
newConsumer
  :: (MonadIO m, MonadIO f, MonadReader PulsarCtx m)
  => Topic
  -> SubscriptionName
  -> m (Consumer f)
newConsumer topic sub = do
  (Ctx conn app _) <- ask
  chan             <- liftIO newChan
  cid              <- mkConsumerId chan app
  fchan            <- liftIO newChan
  let acquire = mkSubscriber conn chan cid app >> forkIO (fetcher chan fchan)
      release = (newReq app >>= C.closeConsumer conn chan cid >>) . killThread
      handler = void $ managed (bracket acquire release)
  addWorker app handler
  return $ Consumer (liftIO $ readChan fchan) (acker conn cid)
 where
  fetcher chan fc = forever $ readChan chan >>= \case
    PayloadResponse cmd _ p -> case cmd ^. F.maybe'message of
      Just msg ->
        let msgId = msg ^. F.messageId
            pm    = Message (MsgId msgId) $ maybe "" (\(Payload x) -> x) p
        in  logResponse cmd >> writeChan fc pm
      Nothing -> return ()
    _ -> return ()
  newReq app = mkRequestId app
  acker conn cid (MsgId mid) = liftIO $ C.ack conn cid mid
  mkSubscriber conn chan cid app = do
    req1 <- newReq app
    C.lookup conn chan req1 topic
    req2 <- newReq app
    C.newSubscriber conn chan req2 cid topic sub
    C.flow conn cid
