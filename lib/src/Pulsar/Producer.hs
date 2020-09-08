{-# LANGUAGE FlexibleContexts #-}

{- |
Module      : Pulsar.Producer
Description : Apache Pulsar client
License     : Apache-2.0
Maintainer  : gabriel.volpe@chatroulette.com
Stability   : experimental

The basic producer interaction looks as follows: http://pulsar.apache.org/docs/en/develop-binary-protocol/#producer

>>> LOOKUP
<<< LOOKUP_RESPONSE
>>> PRODUCER
<<< SUCCESS
>>> SEND 1
>>> SEND 2
<<< SEND_RECEIPT 1
<<< SEND_RECEIPT 2

When the program finishes, either succesfully or due to a failure, we close the producer.

>>> CLOSE_PRODUCER
<<< SUCCESS
-}
module Pulsar.Producer where

import           Control.Concurrent.Async       ( async )
import           Control.Concurrent.Chan
import           Control.Monad.Catch            ( bracket_ )
import           Control.Concurrent.MVar
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import           Control.Monad.Managed          ( managed_
                                                , runManaged
                                                )
import           Control.Monad.Reader           ( MonadReader
                                                , ask
                                                )
import           Data.IORef
import           Data.Text                      ( Text )
import qualified Pulsar.Core                   as C
import           Pulsar.Connection
import           Pulsar.Types

{- | An abstract 'Producer' able to 'send' messages of type 'PulsarMessage'. -}
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
  var              <- liftIO newEmptyMVar
  let release = newReq app >>= C.closeProducer conn chan pid
      handler = managed_ (bracket_ (pure ()) release) >> liftIO (readMVar var)
  worker <- liftIO $ async (runManaged handler)
  addWorker app (worker, var)
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
