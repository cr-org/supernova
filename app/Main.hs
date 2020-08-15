{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.Managed          ( with )
import           Prelude                 hiding ( lookup )
import           Pulsar

main :: IO ()
main = with (connect defaultConnectData) $ \conn ->
  runPulsar conn $ do
    ping
    lookup topic
    newProducer topic
    send "foo"
    closeProducer

topic :: Topic
topic = defaultTopic "app"

concrete :: Pulsar ()
concrete = do
  ping
  newSubscriber topic (SubscriptionName "test-sub")

abstract :: MonadPulsar m => m ()
abstract = liftPulsar $ ping >> newProducer topic
