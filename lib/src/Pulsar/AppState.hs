{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell #-}

module Pulsar.AppState where

import           Control.Concurrent.Async       ( Async )
import           Control.Concurrent.Chan.Unagi
import           Control.Concurrent.MVar
import           Control.Monad.IO.Class
import qualified Data.Binary                   as B
import           Data.Foldable                  ( traverse_ )
import           Data.IORef
import           Lens.Family
import           Lens.Family.TH
import           Pulsar.Protocol.Frame          ( Response(..) )

newtype ReqId = ReqId B.Word64 deriving (Eq, Num, Show)
newtype SeqId = SeqId B.Word64 deriving (Eq, Num, Show)
newtype ProducerId = PId B.Word64 deriving (Eq, Num, Show)
newtype ConsumerId = CId B.Word64 deriving (Eq, Num, Show)

newtype Permits = Permits B.Word32 deriving (Eq, Num, Show)

{- | It represents a running worker in the background along with a synchronizer. -}
type Worker = (Async (), MVar ())

{- | It represents a list of registered sequence ids for a producer. -}
type ProducerSeqs = [(SeqId, MVar Response)]

data AppState = AppState
  { _appConsumers :: [(ConsumerId, InChan Response)]  -- a list of consumer identifiers associated with a communication channel
  , _appConsumerId :: ConsumerId                      -- an incremental counter to assign unique consumer ids
  , _appProducerId :: ProducerId                      -- an incremental counter to assign unique producer ids
  , _appRequestId :: ReqId                            -- an incremental counter to assign unique request ids for all commands
  , _appWorkers :: [Worker]                           -- a list of workers for consumers and producers that run in the background
  , _appResponse :: [(ReqId, MVar Response)]          -- a list of registered requests that need a Request Id
  , _appSendReceipts :: [(ProducerId, ProducerSeqs)]  -- a list of registered messages sent by a specific producer
  }
$(makeLenses ''AppState)

mkConsumerId :: MonadIO m => InChan Response -> IORef AppState -> m ConsumerId
mkConsumerId chan ref = liftIO $ atomicModifyIORef ref $ \app ->
  let cid = app ^. appConsumerId
      f   = over appConsumers ((cid, chan) :) app
  in  (over appConsumerId (+ 1) f, cid)

mkProducerId :: MonadIO m => IORef AppState -> m ProducerId
mkProducerId ref = liftIO $ atomicModifyIORef ref $ \app ->
  let pid = app ^. appProducerId
      f   = over appSendReceipts ((pid, []) :) app
  in  (over appProducerId (+ 1) f, pid)

mkRequestId :: MonadIO m => IORef AppState -> m (ReqId, MVar Response)
mkRequestId ref = liftIO $ do
  var <- newEmptyMVar
  atomicModifyIORef ref $ \app ->
    let req = app ^. appRequestId
        f   = over appResponse ((req, var) :) app
    in  (over appRequestId (+ 1) f, (req, var))

addWorker :: MonadIO m => IORef AppState -> (Async (), MVar ()) -> m ()
addWorker ref nw =
  liftIO $ atomicModifyIORef ref $ \app -> (over appWorkers (nw :) app, ())

{- | Register a response for a request and unregister request. -}
registerReqResponse :: MonadIO m => IORef AppState -> ReqId -> Response -> m ()
registerReqResponse ref rid resp = liftIO $ do
  maybeVar <- atomicModifyIORef ref
    $ \app -> (over appResponse h app, lookup rid $ app ^. appResponse)
  traverse_ (`putMVar` resp) maybeVar
  where h = filter ((rid /=) . fst) -- unregister request

getProducerSeqs pid xs = filter (\(p, _) -> pid == p) xs >>= snd

updateProducerSeqs pid g xs =
  (\(p, ys) -> if p == pid then (p, g) else (p, ys)) <$> xs

registerSeqId
  :: MonadIO m => IORef AppState -> ProducerId -> SeqId -> m (MVar Response)
registerSeqId ref pid sid = liftIO $ do
  var <- newEmptyMVar
  atomicModifyIORef ref $ \app ->
    let xs = app ^. appSendReceipts
        g  = (sid, var) : getProducerSeqs pid xs
        h  = updateProducerSeqs pid g xs
    in  (set appSendReceipts h app, var)

{- | Register a response for a message sent and unregister sequence id. -}
registerSendReceipt
  :: MonadIO m => IORef AppState -> ProducerId -> SeqId -> Response -> m ()
registerSendReceipt ref pid sid resp = liftIO $ do
  maybeVar <- atomicModifyIORef ref
    $ \app -> (over appSendReceipts h app, g $ app ^. appSendReceipts)
  traverse_ (`putMVar` resp) maybeVar
 where
  h xs =
    let f = filter (\(s, _) -> s /= sid) $ getProducerSeqs pid xs
    in  updateProducerSeqs pid f xs
  g = lookup sid . getProducerSeqs pid
