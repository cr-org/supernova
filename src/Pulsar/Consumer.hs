{-# LANGUAGE LambdaCase #-}

module Pulsar.Consumer where

import qualified Control.Monad.Catch           as E
import           Control.Monad.Managed
import           Lens.Family
import           Proto.PulsarApi                ( CommandMessage )
import qualified Proto.PulsarApi_Fields        as F
import qualified Pulsar.Core                   as C
import           Pulsar.Connection
import           Pulsar.Types
import           UnliftIO.Async                 ( concurrently )
import           UnliftIO.Chan

data Consumer m = Consumer
  { fetch :: m CommandMessage
  , ack :: CommandMessage -> m ()
  }

newConsumer
  :: (MonadManaged m, MonadIO f)
  => PulsarCtx
  -> Topic
  -> SubscriptionName
  -> m (Consumer f)
newConsumer (Ctx conn@(Conn s) cs _) topic sub = do
  chan <- newChan
  cid  <- getSetId cs
  using $ Consumer (liftIO $ fetcher chan) (liftIO . C.ack conn cid) <$ managed
    (E.bracket (mkSubscriber cid) (const $ C.closeConsumer conn cid))
 where
  fetcher chan = snd <$> concurrently (consume chan) (readChan chan)
  mkSubscriber cid = do
    C.lookup conn topic
    C.newSubscriber conn cid topic sub
    C.flow conn cid
  recv = do
    resp <- receive s
    C.logResponse resp
    return $ getCommand resp ^. F.maybe'message
  consume chan = recv >>= \case
    Just msg -> writeChan chan msg
    Nothing  -> return ()
