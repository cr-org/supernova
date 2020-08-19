{-# LANGUAGE OverloadedStrings #-}

module Pulsar.Internal.Logger where

import           Control.Logging                ( debug' )
import           Control.Monad.IO.Class         ( MonadIO )
import qualified Data.Text                     as T

logRequest :: (MonadIO m, Show a) => a -> m ()
logRequest cmd = debug' $ ">>> " <> T.pack (show cmd)

logResponse :: (MonadIO m, Show a) => a -> m ()
logResponse cmd = debug' $ "<<< " <> T.pack (show cmd)
