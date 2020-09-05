{-# LANGUAGE FlexibleContexts #-}

{- Defines a high-level Pulsar producer for the end user -}
module Pulsar.Producer where

import           Control.Concurrent.Chan
import           Control.Monad.Catch            ( bracket_ )
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import           Control.Monad.Managed          ( managed_ )
import           Control.Monad.Reader           ( MonadReader
                                                , ask
                                                )
import           Data.IORef
import           Data.Text                      ( Text )
import qualified Pulsar.Core                   as C
import           Pulsar.Connection
import           Pulsar.Types

{- | An abstract 'Producer' able to 'produce' messages of type 'PulsarMessage'. -}
newtype Producer m = Producer
  { send :: PulsarMessage -> m () -- ^ Produces a single message.
  }

data ProducerState = ProducerState
  { stSeqId :: SeqId -- an incremental message sequence counter
  , stName :: Text   -- a unique name
  }

mkSeqId :: MonadIO m => IORef ProducerState -> m SeqId
mkSeqId ref = liftIO $ atomicModifyIORef
  ref
  (\(ProducerState s n) -> let s' = s + 1 in (ProducerState s' n, s))

{- | Create a new 'Producer' by supplying a 'PulsarCtx' (returned by 'Pulsar.connect') and a 'Topic'. -}
newProducer
  :: (MonadIO m, MonadReader PulsarCtx m, MonadIO f) => Topic -> m (Producer f)
newProducer topic = do
  (Ctx conn app _) <- ask
  chan             <- liftIO newChan
  pid              <- mkProducerId chan app
  pname            <- liftIO $ mkProducer conn chan pid app
  pst              <- liftIO $ newIORef (ProducerState 0 pname)
  let release = newReq app >>= C.closeProducer conn chan pid
      handler = managed_ $ bracket_ (pure ()) release
  addWorker app handler
  return $ Producer (dispatch conn chan pid pst)
 where
  newReq app = mkRequestId app
  dispatch conn chan pid pst msg = do
    sid <- mkSeqId pst
    liftIO $ C.send conn chan pid sid msg
  mkProducer conn chan pid app = do
    req1 <- newReq app
    C.lookup conn chan req1 topic
    req2 <- newReq app
    C.newProducer conn chan req2 pid topic
