{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Pulsar.Connection where

import           Control.Applicative            ( (<|>) )
import           Control.Concurrent             ( forkIO
                                                , killThread
                                                , threadDelay
                                                )
import           Control.Concurrent.Async       ( async
                                                , concurrently_
                                                )
import           Control.Concurrent.Chan
import           Control.Concurrent.MVar
import           Control.Exception              ( throwIO )
import           Control.Monad                  ( forever
                                                , when
                                                )
import           Control.Monad.Catch            ( MonadThrow
                                                , bracket
                                                )
import           Control.Monad.IO.Class
import           Control.Monad.Managed          ( MonadManaged
                                                , managed
                                                , runManaged
                                                )
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
import           Pulsar.AppState
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
initAppState = liftIO . newIORef $ AppState [] 0 0 0 [] [] []

responseForRequest :: BaseCommand -> Maybe ReqId
responseForRequest cmd =
  let cmd1 = view F.requestId <$> cmd ^. F.maybe'success
      cmd2 = view F.requestId <$> cmd ^. F.maybe'producerSuccess
      cmd3 = view F.requestId <$> cmd ^. F.maybe'lookupTopicResponse
  in  ReqId <$> (cmd1 <|> cmd2 <|> cmd3)

responseForSendReceipt :: BaseCommand -> Maybe (ProducerId, SeqId)
responseForSendReceipt cmd =
  let cmd' = cmd ^. F.maybe'sendReceipt
      pid  = PId . view F.producerId <$> cmd'
      sid  = SeqId . view F.sequenceId <$> cmd'
  in  (,) <$> pid <*> sid

pongResponse :: BaseCommand -> Chan BaseCommand -> IO (Maybe ())
pongResponse cmd chan =
  traverse (const $ writeChan chan cmd) (cmd ^. F.maybe'pong)

messageResponse :: BaseCommand -> Maybe ConsumerId
messageResponse cmd =
  let cmd' = cmd ^. F.maybe'message
      cid  = view F.consumerId <$> cmd'
  in  CId <$> cid

{- | It listens to incoming messages directly from the network socket and it writes them to all the
 - consumers and producers' communication channels. -}
recvDispatch :: NS.Socket -> IORef AppState -> Chan BaseCommand -> IO ()
recvDispatch s ref chan = forever $ do
  resp <- receive s
  cs   <- appConsumers <$> readIORef ref
  let
    f = \rid -> registerReqResponse ref rid resp
    g = (\(pid, sid) -> registerSendReceipt ref pid sid resp)
    h = \cid ->
      traverse (\(cid', cn) -> when (cid == cid') (writeChan cn resp)) cs
    cmd = getCommand resp
  traverse_ f (responseForRequest cmd)
  traverse_ g (responseForSendReceipt cmd)
  traverse_ h (messageResponse cmd)
  pongResponse cmd chan

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
