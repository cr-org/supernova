{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Pulsar.Consumer where

import           Control.Monad                  ( forever )
import qualified Control.Monad.Catch           as E
import           Control.Monad.Managed
import qualified Data.ByteString.Lazy.Char8    as CL
import qualified Data.Text                     as T
import           Lens.Family
import           Proto.PulsarApi                ( CommandMessage )
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

data Consumer m a = Consumer
  { fetch :: m a
  , ack :: CommandMessage -> m ()
  }

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
    (E.bracket (mkSubscriber chan cid >> forkIO (fetcher chan fchan))
               (\i -> C.closeConsumer conn chan cid >> killThread i)
    )
 where
  fetcher app fc = liftIO . forever $ readChan app >>= \case
    PayloadResponse cmd _ p -> case cmd ^. F.maybe'message of
      Just msg ->
        let pm = Message msg $ maybe "" (\(Payload x) -> x) p
        in  logResponse msg >> writeChan fc pm
      Nothing -> return ()
    _ -> return ()
  acker cid = liftIO . C.ack conn cid
  mkSubscriber chan cid = do
    C.lookup conn chan topic
    C.newSubscriber conn chan cid topic sub
    C.flow conn cid
