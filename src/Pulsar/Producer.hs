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
  { stSeqId :: SeqId -- an incremental message sequence counter
  , stName :: Text   -- a unique name
  }

mkSeqId :: MonadIO m => IORef ProducerState -> m SeqId
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
               (const $ newReq >>= \r -> C.closeProducer conn chan r pid)
    )
 where
  newReq = mkRequestId app
  dispatch chan pid pst msg = do
    sid <- mkSeqId pst
    liftIO $ C.send conn chan pid sid msg
  mkProducer chan pid = do
    req1 <- newReq
    C.lookup conn chan req1 topic
    req2 <- newReq
    C.newProducer conn chan req2 pid topic
