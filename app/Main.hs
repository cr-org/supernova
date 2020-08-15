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
    sleep 2 -- instead of a sleep we should interpret the lookup response
    newProducer topic
    sleep 2 -- ditto
    send "foo"
    closeProducer

sleep :: Int -> Pulsar ()
sleep n = liftIO $ threadDelay (n * 1000000)

topic :: Topic
topic = defaultTopic "app"

concrete :: Pulsar ()
concrete = do
  ping
  newSubscriber topic (SubscriptionName "test-sub")

abstract :: MonadPulsar m => m ()
abstract = liftPulsar $ ping >> newProducer topic
