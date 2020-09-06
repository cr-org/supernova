{-# LANGUAGE GeneralizedNewtypeDeriving, LambdaCase #-}

{- Defines a Pulsar Monad, which wraps a ReaderT and runs internal computations in the background -}
module Pulsar.Internal.Core where

import           Control.Concurrent.Async       ( cancel )
import           Control.Concurrent.MVar
import qualified Control.Logging               as L
import           Control.Monad.Catch            ( MonadThrow
                                                , finally
                                                , throwM
                                                )
import           Control.Monad.Managed
import           Control.Monad.Reader
import           Data.Foldable                  ( traverse_ )
import           Data.IORef                     ( readIORef )
import           Pulsar.Connection              ( AppState(..)
                                                , PulsarCtx(..)
                                                )

{- | Pulsar connection monad, which abstracts over a 'Managed' monad. -}
newtype Connection a = Connection (Managed a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadManaged)

instance MonadThrow Connection where
  throwM = liftIO . throwM

{- | Alias for Connection PulsarCtx. -}
type PulsarConnection = Connection PulsarCtx

{- | The main Pulsar monad, which abstracts over a 'ReaderT' monad. -}
newtype Pulsar a = Pulsar (ReaderT PulsarCtx IO a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader PulsarCtx)

{- | Runs a Pulsar computation with default logging to standard output -}
runPulsar :: PulsarConnection -> Pulsar a -> IO ()
runPulsar = runPulsar' (LogOptions Debug StdOut)

{- | Runs a Pulsar computation with the supplied logging options -}
runPulsar' :: LogOptions -> PulsarConnection -> Pulsar a -> IO ()
runPulsar' (LogOptions lvl out) (Connection mgd) (Pulsar mr) = do
  L.setLogLevel $ fromLogLevel lvl
  L.setLogTimeFormat "%H:%M:%S%Q"
  case out of
    StdOut  -> L.withStdoutLogging runner
    File fp -> L.withFileLogging fp runner
 where
  runner = runManaged $ do
    ctx <- mgd
    void . liftIO $ runReaderT mr ctx `finally` finalizers ctx
  finalizers ctx = do
    let (worker, connVar) = ctxConnWorker ctx
    app <- readIORef (ctxState ctx)
    traverse_ (\(a, v) -> putMVar v () >> cancel a) (appWorkers app)
      `finally` putMVar connVar ()
    cancel worker

{- | Internal logging options. Can be used together with `runPulsar'`. -}
data LogOptions = LogOptions
  { logLevel :: LogLevel
  , logOutput :: LogOutput
  } deriving Show

{- | Internal logging level, part of 'LogOptions'. Can be used together with `runPulsar'`. -}
data LogLevel = Error | Warn | Info | Debug deriving Show

{- | Internal logging output, part of 'LogOptions'. Can be used together with `runPulsar'`. -}
data LogOutput = StdOut | File FilePath deriving Show

fromLogLevel :: LogLevel -> L.LogLevel
fromLogLevel Error = L.LevelError
fromLogLevel Warn  = L.LevelWarn
fromLogLevel Info  = L.LevelInfo
fromLogLevel Debug = L.LevelDebug
