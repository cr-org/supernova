{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Main where

import           Control.Concurrent             ( threadDelay )
import           Control.Concurrent.Async       ( concurrently_ )
import           Control.Monad                  ( forever )
import           Data.Foldable                  ( traverse_ )
import           Pulsar

main :: IO ()
main = runPulsar resources $ \(Consumer {..}, Producer {..}) ->
  let c = forever $ fetch >>= \m -> print m >> ack m
      p = traverse_ produce ["foo", "bar", "taz"]
  in  concurrently_ c p

topic :: Topic
topic = defaultTopic "app"

resources :: Pulsar (Consumer IO, Producer IO)
resources = do
  ctx      <- connect defaultConnectData
  consumer <- newConsumer ctx topic "test-sub"
  producer <- newProducer ctx topic
  return (consumer, producer)

sleep :: Int -> IO ()
sleep n = threadDelay (n * 1000000)
