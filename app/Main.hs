{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Main where

import           Control.Concurrent             ( threadDelay )
import           Control.Concurrent.Async       ( concurrently_ )
import           Control.Logging                ( LogLevel(..) )
import           Control.Monad                  ( forever )
import           Data.Foldable                  ( traverse_ )
import           Pulsar
import           Streamly
import qualified Streamly.Prelude              as S

main :: IO ()
main = streamDemo

topic :: Topic
topic = defaultTopic "app"

demo :: IO ()
demo = runPulsar' LevelInfo resources $ \(Consumer {..}, Producer {..}) ->
  let c = forever $ fetch >>= \m@(Message i _) -> print m >> ack i
      p = forever $ sleep 5 >> traverse_ produce ["foo", "bar", "taz"]
  in  concurrently_ c p

resources :: Pulsar (Consumer IO Message, Producer IO)
resources = do
  ctx      <- connect defaultConnectData
  consumer <- newConsumer ctx topic "test-sub"
  producer <- newProducer ctx topic
  return (consumer, producer)

sleep :: Int -> IO ()
sleep n = threadDelay (n * 1000000)

streamDemo :: IO ()
streamDemo = runPulsar resources $ \(Consumer {..}, Producer {..}) ->
  let c = forever $ fetch >>= \m@(Message i _) -> print m >> ack i
      p = forever $ sleep 5 >> traverse_ produce ["foo", "bar", "taz"]
  in  S.drain . asyncly . maxThreads 10 $ S.yieldM c <> S.yieldM p
