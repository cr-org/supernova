{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Pulsar.Internal.Core where

import           Control.Monad.Reader
import           Data.IORef ( IORef )
import           Pulsar.Connection

newtype Pulsar a = Pulsar (ReaderT PulsarCtx IO a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader PulsarCtx)

data PulsarCtx = Ctx
  { ctxConn :: Connection
  , ctxProducers :: IORef [Int]
  }
