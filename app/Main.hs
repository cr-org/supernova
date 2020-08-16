{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.IO.Class         ( liftIO )
import           Control.Concurrent             ( threadDelay )
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
    newSubscriber topic "test-sub"
    sleep 2
    closeProducer
    closeConsumer

sleep :: Int -> Pulsar ()
sleep n = liftIO $ threadDelay (n * 1000000)

topic :: Topic
topic = defaultTopic "app"

concrete :: Pulsar ()
concrete = do
  ping
  newSubscriber topic "test-sub"

abstract :: MonadPulsar m => m ()
abstract = liftPulsar $ ping >> newProducer topic
