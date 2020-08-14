{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.IO.Class         ( liftIO )
import           Control.Concurrent             ( threadDelay )
import           Control.Monad.Managed          ( with )
import           Pulsar

main :: IO ()
main = with (connect defaultConnectData) $ \conn ->
  runPulsar conn $ do
    ping
    newProducer topic
    liftIO $ threadDelay 2000000
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
