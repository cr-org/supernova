{-# LANGUAGE RankNTypes, GeneralizedNewtypeDeriving #-}

module Pulsar.Internal.Core where

import           Control.Monad.Catch
import           Control.Monad.Managed

newtype Pulsar a = Pulsar (Managed a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadManaged, MonadThrow)

runPulsar :: forall a b. Pulsar a -> (a -> IO b) -> IO b
runPulsar (Pulsar mgd) = with mgd

instance MonadThrow Managed where
  throwM = liftIO . throwM
