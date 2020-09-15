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
import           Control.Concurrent.Chan.Unagi
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
import           Pulsar.AppState
import           Pulsar.Connection              ( PulsarCtx(..) )
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
  (cin, cout)      <- liftIO newChan
  cid              <- mkConsumerId cin app
  (fin, fout)      <- liftIO newChan
  ref              <- liftIO $ newIORef 0
  var              <- liftIO newEmptyMVar
  let permits = issuePermits conn cid
      acquire = do
        mkSubscriber conn cid app
        forkIO (fetcher cout fin ref permits)
      release i =
        killThread i >> newReq app >>= \(r, v) -> C.closeConsumer conn v cid r
      handler = managed (bracket acquire release) >> liftIO (readMVar var)
  worker <- liftIO $ async (runManaged $ void handler)
  addWorker app (worker, var)
  return $ Consumer (liftIO $ readChan fout) (acker conn cid)
 where
  newReq app = mkRequestId app
  acker conn cid (MsgId mid) = liftIO $ C.ack conn cid mid
  issuePermits conn cid =
    C.flow conn cid (Permits $ fromIntegral (defaultQueueSize `div` 2))
  mkSubscriber conn cid app = do
    (req1, var1) <- newReq app
    C.lookup conn var1 req1 topic
    (req2, var2) <- newReq app
    C.newSubscriber conn var2 req2 cid topic sub
    C.flow conn cid (Permits $ fromIntegral defaultQueueSize)

{- | It reads responses from the main communication channel and whenever it corresponds to a
 - 'PayloadResponse', it creates a 'Message' and it writes it to the fetcher channel, which
 - is the one the 'fetch' function is listening on.
 -
 - It also keeps count of the internal fetcher channel size and issues new permits (FLOW)
 - whenever necessary.
 -}
fetcher :: OutChan Response -> InChan Message -> IORef Int -> IO a -> IO b
fetcher cout fcin ref f = forever $ readChan cout >>= \case
  PayloadResponse cmd _ p -> for_ (cmd ^. F.maybe'message) $ \msg -> do
    let msgId = msg ^. F.messageId
        pm    = Message (MsgId msgId) $ maybe "" (\(Payload x) -> x) p
        reset = updateQueueSize ref ((defaultQueueSize `div` 2) -)
    logResponse cmd
    updateQueueSize ref (+ 1)
    size <- readIORef ref
    when (size >= defaultQueueSize `div` 2) (f >> reset)
    writeChan fcin pm
  _ -> return ()
