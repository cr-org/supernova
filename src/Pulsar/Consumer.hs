{-# LANGUAGE LambdaCase #-}

module Pulsar.Consumer where

import           Control.Monad                  ( forever )
import qualified Control.Monad.Catch           as E
import           Control.Monad.Managed
import qualified Data.Binary                   as B
import           Data.Text                      ( Text )
import           Lens.Family
import           Proto.PulsarApi                ( BaseCommand
                                                , CommandMessage
                                                )
import qualified Proto.PulsarApi_Fields        as F
import qualified Pulsar.Core                   as C
import           Pulsar.Connection
import           Pulsar.Protocol.Frame          ( Payload
                                                , Response(..)
                                                )
import           Pulsar.Types
import           UnliftIO.Async                 ( async
                                                , concurrently
                                                , wait
                                                , withAsync
                                                )
import           UnliftIO.Chan
import           UnliftIO.Concurrent

data Msg = Msg CommandMessage (Maybe Payload) deriving Show

data Consumer m a = Consumer
  { fetch :: m a
  , ack :: CommandMessage -> m ()
  }

newConsumer
  :: (MonadManaged m, MonadIO f)
  => PulsarCtx
  -> Topic
  -> SubscriptionName
  -> m (Consumer f Msg)
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
      Just msg -> writeChan fc (Msg msg p)
      Nothing  -> return ()
    _ -> return ()
  acker cid = liftIO . C.ack conn cid
  mkSubscriber chan cid = do
    C.lookup conn chan topic
    C.newSubscriber conn chan cid topic sub
    C.flow conn cid
