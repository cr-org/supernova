{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Main where

import           Control.Concurrent             ( threadDelay )
import           Control.Concurrent.Async       ( concurrently_ )
import           Control.Monad                  ( forever )
import           Control.Monad.IO.Class         ( liftIO )
import           Control.Monad.Managed          ( Managed
                                                , with
                                                )
import           Data.Foldable                  ( traverse_ )
import           Pulsar

main :: IO ()
main = with resources $ \(Consumer {..}, Producer {..}) ->
  let c = forever $ fetch >>= \m -> print m >> ack m
      p = traverse_ produce ["foo", "bar", "taz", "nop", "yay"] >> sleep 5
  in  concurrently_ c p

resources :: Managed (Consumer IO, Producer IO)
resources = do
  ctx      <- connect defaultConnectData
  consumer <- newConsumer ctx topic "test-sub"
  producer <- newProducer ctx topic
  return (consumer, producer)

sleep :: Int -> IO ()
sleep n = threadDelay (n * 1000000)

topic :: Topic
topic = defaultTopic "app"
