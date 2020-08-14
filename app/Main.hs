{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Pulsar

main :: IO ()
main = runPulsar conn example
  where conn = connect defaultConnectData

topic :: Topic
topic = defaultTopic "test"

example :: Pulsar ()
example = do
  ping
  producer topic
  subscribe topic (SubscriptionName "test-sub")

abstract :: MonadPulsar m => m ()
abstract = liftPulsar $ ping >> producer topic
