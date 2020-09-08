{-# LANGUAGE FlexibleContexts, LambdaCase, OverloadedStrings #-}

{- |
Module      : Pulsar.Consumer
Description : Apache Pulsar client
License     : Apache-2.0
Maintainer  : gabriel.volpe@chatroulette.com
Stability   : experimental

The basic consumer interaction looks as follows: http://pulsar.apache.org/docs/en/develop-binary-protocol/#consumer

>>> LOOKUP
<<< LOOKUP_RESPONSE
>>> SUBSCRIBE
<<< SUCCESS
>>> FLOW 1000
<<< MESSAGE 1
<<< MESSAGE 2
>>> ACK 1
>>> ACK 2

When half of the messages have been consumed from our internal queue (Chan), we ask the broker to send more events and continue processing events.

>>> FLOW 500

When the program finishes, either succesfully or due to a failure, we unsubscribe and close the consumer.

>>> CLOSE_CONSUMER
<<< SUCCESS
-}
module Pulsar.Consumer
  ( Consumer(..)
  , newConsumer
  )
where

import           Control.Concurrent             ( forkIO
                                                , killThread
                                                )
import           Control.Concurrent.Async       ( async )
import           Control.Concurrent.Chan
import           Control.Concurrent.MVar
import           Control.Monad                  ( forever
                                                , when
                                                )
import           Control.Monad.Catch            ( bracket )
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import           Control.Monad.Managed          ( managed
                                                , runManaged
                                                )
import           Control.Monad.Reader           ( MonadReader
                                                , ask
                                                )
import           Data.Foldable                  ( for_ )
import           Data.IORef
import           Data.Functor                   ( void )
import           Lens.Family             hiding ( reset )
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

{- | The protocol expects the implementation to use some kind of queue to store events sent by the broker. -}
defaultQueueSize :: Int
defaultQueueSize = 1000

{- | It keeps track of the size of our internal messages queue . -}
updateQueueSize :: IORef Int -> (Int -> Int) -> IO ()
updateQueueSize ref f = atomicModifyIORef ref (\x -> (f x, ()))

{- | Create a new 'Consumer' by supplying a 'PulsarCtx' (returned by 'Pulsar.connect'), a 'Topic' and a 'SubscriptionName'. -}
newConsumer
  :: (MonadIO m, MonadIO f, MonadReader PulsarCtx m)
  => Topic
  -> Subscription
  -> m (Consumer f)
newConsumer topic sub = do
  (Ctx conn app _) <- ask
  chan             <- liftIO newChan
  cid              <- mkConsumerId chan app
  fchan            <- liftIO newChan
  ref              <- liftIO $ newIORef 0
  var              <- liftIO newEmptyMVar
  let permits = issuePermits conn cid
      acquire = do
        mkSubscriber conn chan cid app
        forkIO (fetcher chan fchan ref permits)
      release i = killThread i >> newReq app >>= C.closeConsumer conn chan cid
      handler = managed (bracket acquire release) >> liftIO (readMVar var)
  worker <- liftIO $ async (runManaged $ void handler)
  addWorker app (worker, var)
  return $ Consumer (liftIO $ readChan fchan) (acker conn cid)
 where
  fetcher chan fc ref f = forever $ readChan chan >>= \case
    PayloadResponse cmd _ p -> for_ (cmd ^. F.maybe'message) $ \msg -> do
      let msgId = msg ^. F.messageId
          pm    = Message (MsgId msgId) $ maybe "" (\(Payload x) -> x) p
          reset = updateQueueSize ref ((defaultQueueSize `div` 2) -)
      logResponse cmd
      updateQueueSize ref (+ 1)
      size <- readIORef ref
      when (size >= defaultQueueSize `div` 2) (f >> reset)
      writeChan fc pm
    _ -> return ()
  newReq app = mkRequestId app
  acker conn cid (MsgId mid) = liftIO $ C.ack conn cid mid
  issuePermits conn cid =
    C.flow conn cid (Permits $ fromIntegral (defaultQueueSize `div` 2))
  mkSubscriber conn chan cid app = do
    req1 <- newReq app
    C.lookup conn chan req1 topic
    req2 <- newReq app
    C.newSubscriber conn chan req2 cid topic sub
    C.flow conn cid (Permits $ fromIntegral defaultQueueSize)
