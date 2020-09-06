{-|
Module      : Pulsar
Description : Apache Pulsar client
License     : Apache-2.0
Maintainer  : gabriel.volpe@chatroulette.com
Stability   : experimental

Consider the following imports (needs the [async](http://hackage.haskell.org/package/async) library).

@
import           Control.Concurrent             ( threadDelay )
import           Control.Concurrent.Async       ( concurrently_ )
import           Control.Monad                  ( forever )
import           Pulsar
@

A quick example of a consumer and producer running concurrently.

@
resources :: Pulsar (Consumer IO, Producer IO)
resources = do
  ctx      <- connect defaultConnectData
  consumer <- newConsumer ctx topic "test-sub"
  producer <- newProducer ctx topic
  return (consumer, producer)
@

A Pulsar connection, consumers, and producers are long-lived resources that are managed accordingly for you. Once the program exits, the resources will be released in the respective order (always opposite to the order of acquisition).

@
main :: IO ()
main = runPulsar resources $ \(Consumer {..}, Producer {..}) ->
  let c = forever $ fetch >>= \(Message i m) -> print m >> ack i
      p = forever $ threadDelay (5 * 1000000) >> produce "hello world"
  in  concurrently_ c p
@
-}
module Pulsar
  ( connect
  , defaultConnectData
  , newConsumer
  , newProducer
  , runPulsar
  , runPulsar'
  , Consumer(..)
  , Producer(..)
  , Pulsar
  , PulsarConnection
  , ConnectData
  , LogLevel(..)
  , LogOptions(..)
  , LogOutput(..)
  , module Pulsar.Types
  )
where

import           Pulsar.Connection
import           Pulsar.Consumer
import           Pulsar.Internal.Core
import           Pulsar.Producer
import           Pulsar.Types
