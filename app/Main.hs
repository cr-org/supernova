module Main where

import           Pulsar

main :: IO ()
main = runPulsar conn example
 where conn = connect defaultConnectData

topic :: Topic
topic = defaultTopic "test"

example :: Pulsar ()
example = ping >> producer topic

abstract :: MonadPulsar m => m ()
abstract = liftPulsar $ ping >> producer topic
