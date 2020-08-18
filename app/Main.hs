{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Main where

import           Control.Concurrent             ( forkIO, threadDelay )
import           Control.Concurrent.Async       ( async, wait, concurrently_ )
import           Control.Monad                  ( forever )
import           Data.Foldable                  ( traverse_ )
import           Data.Functor                   ( void )
import           Pulsar

main :: IO ()
main = runPulsar resources $ \(Consumer {..}, Producer {..}) ->
  let c = forever $ fetch >>= \msg@(Msg m _) -> print msg >> ack m
      p = forever $ sleep 5 >> traverse_ produce ["foo", "bar", "taz"]
  in  concurrently_ c p

topic :: Topic
topic = defaultTopic "app"

resources :: Pulsar (Consumer IO Msg, Producer IO)
resources = do
  ctx      <- connect defaultConnectData
  consumer <- newConsumer ctx topic "test-sub"
  producer <- newProducer ctx topic
  return (consumer, producer)

sleep :: Int -> IO ()
sleep n = threadDelay (n * 1000000)
