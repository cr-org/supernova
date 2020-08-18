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

-- a should be the serialized payload
data Msg a = Msg CommandMessage a deriving Show

data Consumer m a = Consumer
  { fetch :: m (Msg a)
  , ack :: CommandMessage -> m ()
  }

newConsumer
  :: (MonadManaged m, MonadIO f)
  => PulsarCtx
  -> Topic
  -> SubscriptionName
  -> m (Consumer f (Maybe Payload))
newConsumer (Ctx conn@(Conn s) app) topic sub = do
  chan  <- newChan
  cid   <- mkConsumerId chan app
  fchan <- newChan
  using $ Consumer (readChan fchan) (acker cid) <$ managed
    (E.bracket (mkSubscriber chan cid >> forkIO (fetcher chan fchan))
               (\i -> C.closeConsumer conn chan cid >> killThread i)
    )
 where
  fetcher app fc = liftIO . forever $ do
    resp <- readChan app
    case getCommand resp ^. F.maybe'message of
      Just msg ->
        case resp of
          PayloadResponse _ _ p ->
            writeChan fc (Msg msg p)
          _ -> return ()
      Nothing  -> return ()
  acker cid = liftIO . C.ack conn cid
  mkSubscriber chan cid = do
    C.lookup conn chan topic
    C.newSubscriber conn chan cid topic sub
    C.flow conn cid
