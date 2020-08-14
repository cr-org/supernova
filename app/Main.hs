{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.Managed          ( with )
import           Pulsar

main :: IO ()
main = with (connect defaultConnectData) $ \conn ->
  runPulsar conn $ do
    ping
    producer topic
    subscribe topic (SubscriptionName "test-sub")

topic :: Topic
topic = defaultTopic "test"

concrete :: Pulsar ()
concrete = ping >> producer topic

abstract :: MonadPulsar m => m ()
abstract = liftPulsar $ ping >> producer topic
