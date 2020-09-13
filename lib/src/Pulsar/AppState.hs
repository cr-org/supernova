{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Pulsar.AppState where

import           Control.Concurrent.Async       ( Async )
import           Control.Concurrent.Chan
import           Control.Concurrent.MVar
import           Control.Monad.IO.Class
import qualified Data.Binary                   as B
import           Data.Foldable                  ( traverse_ )
import           Data.IORef
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

-- TODO: Define lenses for AppState to make reads / writes much more bearable.
data AppState = AppState
  { appConsumers :: [(ConsumerId, Chan Response)]    -- a list of consumer identifiers associated with a communication channel
  , appConsumerId :: ConsumerId                      -- an incremental counter to assign unique consumer ids
  , appProducerId :: ProducerId                      -- an incremental counter to assign unique producer ids
  , appRequestId :: ReqId                            -- an incremental counter to assign unique request ids for all commands
  , appWorkers :: [Worker]                           -- a list of workers for consumers and producers that run in the background
  , appResponse :: [(ReqId, MVar Response)]          -- a list of registered requests that need a Request Id
  , appSendReceipts :: [(ProducerId, ProducerSeqs)]  -- a list of registered messages sent by a specific producer
  }

mkConsumerId :: MonadIO m => Chan Response -> IORef AppState -> m ConsumerId
mkConsumerId chan ref = liftIO $ atomicModifyIORef
  ref
  (\(AppState cs cid pid rid w rs s) ->
    let cid' = cid + 1
    in  (AppState ((cid, chan) : cs) cid' pid rid w rs s, cid)
  )

mkProducerId :: MonadIO m => IORef AppState -> m ProducerId
mkProducerId ref = liftIO $ atomicModifyIORef
  ref
  (\(AppState cs cid pid rid w rs s) ->
    let pid' = pid + 1 in (AppState cs cid pid' rid w rs ((pid, []) : s), pid)
  )

registerSeqId
  :: MonadIO m => IORef AppState -> ProducerId -> SeqId -> m (MVar Response)
registerSeqId ref pid sid = liftIO $ do
  var <- newEmptyMVar
  atomicModifyIORef
    ref
    (\(AppState cs cid pd req w rs xs) ->
      let foo = filter (\(p, _) -> pid == p) xs >>= snd
          bar = (sid, var) : foo
          taz = (\(p, ys) -> if p == pid then (p, bar) else (p, ys)) <$> xs
      in  (AppState cs cid pd req w rs taz, var)
    )

mkRequestId :: MonadIO m => IORef AppState -> m (ReqId, MVar Response)
mkRequestId ref = liftIO $ do
  var <- newEmptyMVar
  atomicModifyIORef
    ref
    (\(AppState cs cid pid req w rs s) ->
      let req' = req + 1
      in  (AppState cs cid pid req' w ((req, var) : rs) s, (req, var))
    )

addWorker :: MonadIO m => IORef AppState -> (Async (), MVar ()) -> m ()
addWorker ref nw = liftIO $ atomicModifyIORef
  ref
  (\(AppState cs cid pid req w rs s) ->
    (AppState cs cid pid req (nw : w) rs s, ())
  )

{- | Register a response for a request and unregister request. -}
registerReqResponse :: MonadIO m => IORef AppState -> ReqId -> Response -> m ()
registerReqResponse ref rid resp = liftIO $ do
  maybeVar <- atomicModifyIORef
    ref
    (\(AppState cs cid pid req w rs s) ->
      (AppState cs cid pid req w (h rs) s, g rs)
    )
  traverse_ (`putMVar` resp) maybeVar
 where
  h = filter ((rid /=) . fst) -- unregister request
  g = lookup rid              -- return MVar to write response

{- | Register a response for a message sent and unregister sequence id. -}
registerSendReceipt
  :: MonadIO m => IORef AppState -> ProducerId -> SeqId -> Response -> m ()
registerSendReceipt ref pid sid resp = liftIO $ do
  maybeVar <- atomicModifyIORef
    ref
    (\(AppState cs cid pid' req w rs s) ->
      (AppState cs cid pid' req w rs (h s), g s)
    )
  traverse_ (`putMVar` resp) maybeVar
 where
  h xs =
    let foo = filter (\(p, _) -> pid == p) xs >>= snd
        bar = filter (\(s, _) -> s /= sid) foo
    in  (\(p, ys) -> if p == pid then (p, bar) else (p, ys)) <$> xs
  g xs = let foo = filter (\(p, _) -> pid == p) xs >>= snd in lookup sid foo
