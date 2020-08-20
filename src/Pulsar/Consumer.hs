{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Pulsar.Consumer where

import           Control.Monad                  ( forever )
import qualified Control.Monad.Catch           as E
import           Control.Monad.Managed
import qualified Data.ByteString.Lazy.Char8    as CL
import qualified Data.Text                     as T
import           Lens.Family
import qualified Proto.PulsarApi_Fields        as F
import qualified Pulsar.Core                   as C
import           Pulsar.Connection
import           Pulsar.Internal.Logger         ( logResponse )
import           Pulsar.Protocol.Frame          ( Payload(..)
                                                , Response(..)
                                                )
import           Pulsar.Types
import           UnliftIO.Chan
import           UnliftIO.Concurrent            ( forkIO
                                                , killThread
                                                )

{- | An abstract 'Consumer' able to 'fetch' messages of type 'a' and 'ack'nowledge them. -}
data Consumer m a = Consumer
  { fetch :: m a
  , ack :: MsgId -> m ()
  }

{- | Create a new 'Consumer' by supplying a 'PulsarCtx' (returned by 'Pulsar.connect'), a 'Topic' and a 'SubscriptionName'. -}
newConsumer
  :: (MonadManaged m, MonadIO f)
  => PulsarCtx
  -> Topic
  -> SubscriptionName
  -> m (Consumer f Message)
newConsumer (Ctx conn@(Conn s) app) topic sub = do
  chan  <- newChan
  cid   <- mkConsumerId chan app
  fchan <- newChan
  using $ Consumer (readChan fchan) (acker cid) <$ managed
    (E.bracket
      (mkSubscriber chan cid >> forkIO (fetcher chan fchan))
      (\i -> newReq >>= \r -> C.closeConsumer conn chan r cid >> killThread i)
    )
 where
  fetcher app fc = liftIO . forever $ readChan app >>= \case
    PayloadResponse cmd _ p -> case cmd ^. F.maybe'message of
      Just msg ->
        let msgId = msg ^. F.messageId
            pm    = Message (MsgId msgId) $ maybe "" (\(Payload x) -> x) p
        in  logResponse cmd >> writeChan fc pm
      Nothing -> return ()
    _ -> return ()
  newReq = mkRequestId app
  acker cid (MsgId mid) = liftIO $ C.ack conn cid mid
  mkSubscriber chan cid = do
    req1 <- newReq
    C.lookup conn chan req1 topic
    req2 <- newReq
    C.newSubscriber conn chan req2 cid topic sub
    C.flow conn cid
