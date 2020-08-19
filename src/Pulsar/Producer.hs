module Pulsar.Producer where

import qualified Control.Monad.Catch           as E
import           Control.Monad.Managed
import qualified Data.Binary                   as B
import           Data.IORef
import           Data.Text                      ( Text )
import qualified Pulsar.Core                   as C
import           Pulsar.Connection
import           Pulsar.Types
import           UnliftIO.Chan

newtype Producer m = Producer
  { produce :: PulsarMessage -> m ()
  }

data ProducerState = ProducerState
  { stSeqId :: B.Word64 -- an incremental message sequence counter
  , stName :: Text      -- a unique name
  }

mkSeqId :: MonadIO m => IORef ProducerState -> m B.Word64
mkSeqId ref = liftIO $ atomicModifyIORef
  ref
  (\(ProducerState s n) -> let s' = s + 1 in (ProducerState s' n, s))

newProducer
  :: (MonadManaged m, MonadIO f) => PulsarCtx -> Topic -> m (Producer f)
newProducer (Ctx conn app) topic = do
  chan  <- newChan
  pid   <- mkProducerId chan app
  pname <- liftIO $ mkProducer chan pid
  pst   <- liftIO $ newIORef (ProducerState 0 pname)
  using $ managed
    (E.bracket (pure $ Producer (dispatch chan pid pst))
               (const $ C.closeProducer conn chan pid)
    )
 where
  dispatch chan pid pst msg = do
    sid <- mkSeqId pst
    liftIO $ C.send conn chan pid sid msg
  mkProducer chan pid = do
    C.lookup conn chan topic
    C.newProducer conn chan pid topic
