{-# LANGUAGE RankNTypes, GeneralizedNewtypeDeriving #-}

{- Defines a Pulsar Monad, which wraps a Managed resource -}
module Pulsar.Internal.Core where

import qualified Control.Logging               as L
import           Control.Monad.Catch
import           Control.Monad.Managed

{- | The main Pulsar monad, which abstracts over a 'Managed' monad. -}
newtype Pulsar a = Pulsar (Managed a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadManaged)

{- | Runs a Pulsar computation with default logging to standard output -}
runPulsar :: forall a b . Pulsar a -> (a -> IO b) -> IO b
runPulsar (Pulsar mgd) f = do
  L.setLogTimeFormat "%H:%M:%S%Q"
  L.withStdoutLogging $ with mgd f

{- | Runs a Pulsar computation with the supplied logging options -}
runPulsar' :: forall a b . LogOptions -> Pulsar a -> (a -> IO b) -> IO b
runPulsar' (LogOptions lvl out) (Pulsar mgd) f = do
  L.setLogLevel $ convertLogLevel lvl
  L.setLogTimeFormat "%H:%M:%S%Q"
  case out of
    StdOut  -> L.withStdoutLogging $ with mgd f
    File fp -> L.withFileLogging fp $ with mgd f

instance MonadThrow Pulsar where
  throwM = liftIO . throwM

{- | Internal logging options. Can be used together with `runPulsar'`. -}
data LogOptions = LogOptions
  { logLevel :: LogLevel
  , logOutput :: LogOutput
  } deriving Show

{- | Internal logging level, part of 'LogOptions'. Can be used together with `runPulsar'`. -}
data LogLevel = Error | Warn | Info | Debug deriving Show

{- | Internal logging output, part of 'LogOptions'. Can be used together with `runPulsar'`. -}
data LogOutput = StdOut | File FilePath deriving Show

convertLogLevel :: LogLevel -> L.LogLevel
convertLogLevel Error = L.LevelError
convertLogLevel Warn  = L.LevelWarn
convertLogLevel Info  = L.LevelInfo
convertLogLevel Debug = L.LevelDebug
