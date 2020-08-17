{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving #-}

module Pulsar.Internal.Core where

import           Control.Monad.Catch
import           Control.Monad.IO.Unlift
import           Control.Monad.Reader
import           Pulsar.Connection              ( PulsarCtx )

newtype Pulsar a = Pulsar (ReaderT PulsarCtx IO a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader PulsarCtx)

deriving instance MonadThrow Pulsar
deriving instance MonadCatch Pulsar
deriving instance MonadMask Pulsar
deriving instance MonadUnliftIO Pulsar
