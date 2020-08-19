{-# LANGUAGE RankNTypes, GeneralizedNewtypeDeriving #-}

module Pulsar.Internal.Core where

import           Control.Logging
import           Control.Monad.Catch
import           Control.Monad.Managed

newtype Pulsar a = Pulsar (Managed a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadManaged, MonadThrow)

runPulsar :: forall a b. Pulsar a -> (a -> IO b) -> IO b
runPulsar (Pulsar mgd) f = do
  setLogTimeFormat "%H:%M:%S%Q"
  withStdoutLogging $ with mgd f

runPulsar' :: forall a b. LogLevel -> Pulsar a -> (a -> IO b) -> IO b
runPulsar' logLvl p f = setLogLevel logLvl >> runPulsar p f

instance MonadThrow Managed where
  throwM = liftIO . throwM
