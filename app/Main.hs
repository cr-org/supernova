{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Main where

import           Control.Monad.IO.Class         ( liftIO )
import           Control.Concurrent             ( forkIO
                                                , threadDelay
                                                )
import           Control.Monad.Managed          ( with )
import           Prelude                 hiding ( lookup )
import           Pulsar

main :: IO ()
main = with (connect defaultConnectData) $ \conn -> runPulsar conn $ do
  ping
  lookup topic
  newSubscriber topic "test-sub"
  lookup topic
  newProducer topic
  liftIO $ runConsumer conn
  send "foo"
  sleep 2
  closeConsumer
  closeProducer
  sleep 1

runConsumer conn = forkIO $ runPulsar conn $ do
  flow >>= \case
    Just msgId -> ack msgId
    Nothing    -> pure ()

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
