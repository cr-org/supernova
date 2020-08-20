{-# LANGUAGE GeneralizedNewtypeDeriving, LambdaCase, OverloadedStrings #-}

module Pulsar.Connection where

import           Control.Monad                  ( forever )
import           Control.Monad.Catch            ( MonadThrow )
import           Control.Monad.Managed
import qualified Data.Binary                   as B
import           Data.Foldable                  ( traverse_ )
import           Data.IORef
import           Data.Text                      ( Text )
import           Lens.Family
import qualified Network.Socket                as NS
import qualified Network.Socket.ByteString.Lazy
                                               as SBL
import           Proto.PulsarApi                ( BaseCommand
                                                , CommandMessage
                                                , CommandPong
                                                , MessageMetadata
                                                , MessageIdData
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
import           UnliftIO.Async                 ( concurrently_ )
import           UnliftIO.Chan
import           UnliftIO.Concurrent            ( forkIO
                                                , killThread
                                                , threadDelay
                                                )
import           UnliftIO.Exception             ( bracket
                                                , throwIO
                                                )

newtype Connection = Conn NS.Socket

newtype ReqId = ReqId B.Word64 deriving (Num, Show)
newtype SeqId = SeqId B.Word64 deriving (Num, Show)
newtype ProducerId = PId B.Word64 deriving (Num, Show)
newtype ConsumerId = CId B.Word64 deriving (Num, Show)

data AppState = AppState
  { appConsumers :: [(ConsumerId, Chan Response)] -- a list of consumer identifiers associated with a communication channel
  , appConsumerId :: ConsumerId                   -- an incremental counter to assign unique consumer ids
  , appProducers :: [(ProducerId, Chan Response)] -- a list of producer identifiers associated with a communication channel
  , appProducerId :: ProducerId                   -- an incremental counter to assign unique producer ids
  , appRequestId :: ReqId                         -- an incremental counter to assign unique request ids for all commands
  }

mkConsumerId :: MonadIO m => Chan Response -> IORef AppState -> m ConsumerId
mkConsumerId chan ref = liftIO $ atomicModifyIORef
  ref
  (\(AppState cs cid ps pid rid) ->
    let cid' = cid + 1 in (AppState ((cid', chan) : cs) cid' ps pid rid, cid)
  )

mkProducerId :: MonadIO m => Chan Response -> IORef AppState -> m ProducerId
mkProducerId chan ref = liftIO $ atomicModifyIORef
  ref
  (\(AppState cs cid ps pid rid) ->
    let pid' = pid + 1 in (AppState cs cid ((pid', chan) : ps) pid' rid, pid)
  )

mkRequestId :: MonadIO m => IORef AppState -> m ReqId
mkRequestId ref = liftIO $ atomicModifyIORef
  ref
  (\(AppState cs cid ps pid req) ->
    let req' = req + 1 in (AppState cs cid ps pid req', req)
  )

data ConnectData = ConnData
    { connHost :: NS.HostName
    , connPort :: NS.ServiceName
    } deriving Show

data PulsarCtx = Ctx
  { ctxConn :: Connection
  , ctxState :: IORef AppState
  }

{- | Default connection data: `localhost:6650` -}
defaultConnectData :: ConnectData
defaultConnectData = ConnData { connHost = "127.0.0.1", connPort = "6650" }

{- | Starts a Pulsar connection with the supplied 'ConnectData' -}
connect
  :: (MonadThrow m, MonadIO m, MonadManaged m) => ConnectData -> m PulsarCtx
connect (ConnData h p) = do
  socket     <- acquireSocket h p
  socketChan <- liftIO newChan
  liftIO $ sendSimpleCmd socket P.connect
  resp <- receive socket
  case getCommand resp ^. F.maybe'connected of
    Just res -> logResponse resp
    Nothing  -> throwIO $ userError "Could not connect"
  app   <- liftIO initAppState
  kchan <- liftIO newChan
  let ctx        = Ctx (Conn socket) app
      dispatcher = recvDispatch socket app kchan
      task       = concurrently_ dispatcher (keepAlive socket kchan)
  using $ ctx <$ managed (bracket (forkIO task) killThread)

initAppState :: MonadIO m => m (IORef AppState)
initAppState = liftIO . newIORef $ AppState [] 0 [] 0 0

recvDispatch
  :: MonadIO m => NS.Socket -> IORef AppState -> Chan BaseCommand -> m ()
recvDispatch s ref chan = forever $ do
  resp                   <- receive s
  (AppState cs _ ps _ _) <- liftIO $ readIORef ref
  case getCommand resp ^. F.maybe'pong of
    Just pong -> writeChan chan (getCommand resp)
    Nothing   -> return ()
  traverse_ (`writeChan` resp) ((snd <$> cs) ++ (snd <$> ps))

{- Emit a PING and expect a PONG every 29 seconds. If a PONG is not received, interrupt connection -}
keepAlive :: MonadIO m => NS.Socket -> Chan BaseCommand -> m ()
keepAlive s chan = forever $ do
  threadDelay (29 * 1000000)
  logRequest P.ping
  sendSimpleCmd s P.ping
  liftIO $ timeout (2 * 1000000) (readChan chan) >>= \case
    Just cmd -> logResponse cmd
    Nothing  -> throwIO (userError "Keep Alive interruption")

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
      Just p -> do
        logResponse $ getCommand resp
        logRequest P.pong
        sendSimpleCmd s P.pong
        return resp
      Nothing -> return resp
