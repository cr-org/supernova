module Pulsar.Producer where

import qualified Control.Monad.Catch           as E
import           Control.Monad.Managed
import qualified Pulsar.Core                   as C
import           Pulsar.Connection
import           Pulsar.Types

newtype Producer m = Producer
  { produce :: PulsarMessage -> m ()
  }

newProducer
  :: (MonadManaged m, MonadIO f) => PulsarCtx -> Topic -> m (Producer f)
newProducer (Ctx conn _ ps) topic = do
  pid <- getSetId ps
  using $ Producer (liftIO . C.send conn pid) <$ managed
    (E.bracket (mkProducer pid) (const $ C.closeProducer conn pid))
 where
  mkProducer pid = do
    C.lookup conn topic
    C.newProducer conn pid topic
