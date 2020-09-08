{-# LANGUAGE GeneralizedNewtypeDeriving, LambdaCase, OverloadedStrings #-}

module Pulsar.Connection where

import           Control.Concurrent             ( forkIO
                                                , killThread
                                                , threadDelay
                                                )
import           Control.Concurrent.Async       ( Async
                                                , async
                                                , concurrently_
                                                )
import           Control.Concurrent.Chan
import           Control.Concurrent.MVar
import           Control.Exception              ( throwIO )
import           Control.Monad                  ( forever )
import           Control.Monad.Catch            ( MonadThrow
                                                , bracket
                                                )
import           Control.Monad.IO.Class
import           Control.Monad.Managed          ( MonadManaged
                                                , managed
                                                , runManaged
                                                )
import qualified Data.Binary                   as B
import           Data.Foldable                  ( traverse_ )
import           Data.Functor                   ( void )
import           Data.IORef
import           Lens.Family
import qualified Network.Socket                as NS
import qualified Network.Socket.ByteString.Lazy
                                               as SBL
import           Proto.PulsarApi                ( BaseCommand
                                                , MessageMetadata
                                                )
import qualified Proto.PulsarApi_Fields        as F
import           Pulsar.Internal.Logger
import           Pulsar.Internal.TCPClient      ( acquireSocket )
import qualified Pulsar.Protocol.Commands      as P
import           Pulsar.Protocol.Decoder        ( decodeBaseCommand )
import           Pulsar.Protocol.Encoder        ( encodeBaseCommand )
import           Pulsar.Protocol.Frame          ( Payload
                                                , Response(..)
                                                , frameMaxSize
                                                , getCommand
                                                )
import           System.Timeout                 ( timeout )

newtype Connection = Conn NS.Socket

newtype ReqId = ReqId B.Word64 deriving (Num, Show)
newtype SeqId = SeqId B.Word64 deriving (Num, Show)
newtype ProducerId = PId B.Word64 deriving (Num, Show)
newtype ConsumerId = CId B.Word64 deriving (Num, Show)

newtype Permits = Permits B.Word32 deriving (Num, Show)

{- | It represents a running worker in the background along with a synchronizer. -}
type Worker = (Async (), MVar ())

data AppState = AppState
  { appConsumers :: [(ConsumerId, Chan Response)] -- a list of consumer identifiers associated with a communication channel
  , appConsumerId :: ConsumerId                   -- an incremental counter to assign unique consumer ids
  , appProducers :: [(ProducerId, Chan Response)] -- a list of producer identifiers associated with a communication channel
  , appProducerId :: ProducerId                   -- an incremental counter to assign unique producer ids
  , appRequestId :: ReqId                         -- an incremental counter to assign unique request ids for all commands
  , appWorkers :: [Worker]                        -- a list of workers for consumers and producers that run in the background
  }

mkConsumerId :: MonadIO m => Chan Response -> IORef AppState -> m ConsumerId
mkConsumerId chan ref = liftIO $ atomicModifyIORef
  ref
  (\(AppState cs cid ps pid rid w) ->
    let cid' = cid + 1 in (AppState ((cid', chan) : cs) cid' ps pid rid w, cid)
  )

mkProducerId :: MonadIO m => Chan Response -> IORef AppState -> m ProducerId
mkProducerId chan ref = liftIO $ atomicModifyIORef
  ref
  (\(AppState cs cid ps pid rid w) ->
    let pid' = pid + 1 in (AppState cs cid ((pid', chan) : ps) pid' rid w, pid)
  )

mkRequestId :: MonadIO m => IORef AppState -> m ReqId
mkRequestId ref = liftIO $ atomicModifyIORef
  ref
  (\(AppState cs cid ps pid req w) ->
    let req' = req + 1 in (AppState cs cid ps pid req' w, req)
  )

addWorker :: MonadIO m => IORef AppState -> (Async (), MVar ()) -> m ()
addWorker ref nw = liftIO $ atomicModifyIORef
  ref
  (\(AppState cs cid ps pid req w) -> (AppState cs cid ps pid req (nw : w), ()))

{- | Connection details: host and port. -}
data ConnectData = ConnData
    { connHost :: NS.HostName
    , connPort :: NS.ServiceName
    } deriving Show

{- | Internal Pulsar context. You will never need to access its content (not exported) but might need to take it as argument. -}
data PulsarCtx = Ctx
  { ctxConn :: Connection
  , ctxState :: IORef AppState
  , ctxConnWorker :: Worker
  }

{- | Default connection data: "127.0.0.1:6650" -}
defaultConnectData :: ConnectData
defaultConnectData = ConnData { connHost = "127.0.0.1", connPort = "6650" }

{- | Starts a Pulsar connection with the supplied 'ConnectData' -}
connect
  :: (MonadIO m, MonadThrow m, MonadManaged m) => ConnectData -> m PulsarCtx
connect (ConnData h p) = do
  socket <- acquireSocket h p
  liftIO $ sendSimpleCmd socket P.connect
  checkConnection socket
  app   <- liftIO initAppState
  kchan <- liftIO newChan
  var   <- liftIO newEmptyMVar
  let
    dispatcher = recvDispatch socket app kchan
    task       = concurrently_ dispatcher (keepAlive socket kchan)
    handler =
      managed (bracket (forkIO task) (\i -> readMVar var >> killThread i))
  worker <- liftIO $ async (runManaged $ void handler)
  return $ Ctx (Conn socket) app (worker, var)

checkConnection :: (MonadIO m, MonadThrow m) => NS.Socket -> m ()
checkConnection socket = do
  resp <- receive socket
  case getCommand resp ^. F.maybe'connected of
    Just _  -> logResponse resp
    Nothing -> liftIO . throwIO $ userError "Could not connect"

initAppState :: MonadIO m => m (IORef AppState)
initAppState = liftIO . newIORef $ AppState [] 0 [] 0 0 []

{- | It listens to incoming messages directly from the network socket and it writes them to all the
 - consumers and producers' communication channels. -}
recvDispatch :: NS.Socket -> IORef AppState -> Chan BaseCommand -> IO ()
recvDispatch s ref chan = forever $ do
  resp                     <- receive s
  (AppState cs _ ps _ _ _) <- readIORef ref
  case getCommand resp ^. F.maybe'pong of
    Just _  -> writeChan chan $ getCommand resp
    Nothing -> traverse_ (`writeChan` resp) ((snd <$> cs) ++ (snd <$> ps))

{- Emit a PING and expect a PONG every 29 seconds. If a PONG is not received, interrupt connection -}
keepAlive :: NS.Socket -> Chan BaseCommand -> IO ()
keepAlive s chan = forever $ do
  threadDelay (29 * 1000000)
  logRequest P.ping
  sendSimpleCmd s P.ping
  timeout (2 * 1000000) (readChan chan) >>= \case
    Just cmd -> logResponse cmd
    Nothing  -> throwIO $ userError "Keep Alive interruption"

sendSimpleCmd :: MonadIO m => NS.Socket -> BaseCommand -> m ()
sendSimpleCmd s cmd =
  liftIO . SBL.sendAll s $ encodeBaseCommand Nothing Nothing cmd

sendPayloadCmd
  :: MonadIO m
  => NS.Socket
  -> BaseCommand
  -> MessageMetadata
  -> Maybe Payload
  -> m ()
sendPayloadCmd s cmd meta payload =
  liftIO . SBL.sendAll s $ encodeBaseCommand (Just meta) payload cmd

receive :: MonadIO m => NS.Socket -> m Response
receive s = liftIO $ do
  msg <- SBL.recv s $ fromIntegral frameMaxSize
  case decodeBaseCommand msg of
    Left  e    -> fail $ "Decoding error: " <> e
    Right resp -> case getCommand resp ^. F.maybe'ping of
      Just _ -> do
        logResponse $ getCommand resp
        logRequest P.pong
        sendSimpleCmd s P.pong
        return resp
      Nothing -> return resp
