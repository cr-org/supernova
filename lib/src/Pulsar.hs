{-|
Module      : Pulsar
Description : Apache Pulsar client
License     : Apache-2.0
Maintainer  : gabriel.volpe@chatroulette.com
Stability   : experimental

In the following example, we will create a quick example showcasing a consumer and producer running concurrently, step by step.

Consider the following imports (needs the [async](http://hackage.haskell.org/package/async) library).

@
import           Control.Concurrent             ( threadDelay )
import           Control.Concurrent.Async       ( concurrently_ )
import           Pulsar
@

Firstly, we create a connection to Pulsar, defined as 'PulsarConnection'.

@
conn :: PulsarConnection
conn = connect defaultConnectData
@

Then a consumer and a producer, which operate in the 'Pulsar' monad.

@
pulsar :: Pulsar ()
pulsar = do
  c <- newConsumer topic sub
  p <- newProducer topic
  liftIO $ program c p
 where
  topic = defaultTopic "app"
  sub   = Subscription Exclusive "test-sub"
@

And the main user program that consume and produce messages concurrently, running in 'IO'.

@
program :: Consumer IO -> Producer IO -> IO ()
program Consumer {..} Producer {..} =
  let c = fetch >>= \(Message i m) -> print m >> ack i >> c
      p = threadDelay (3 * 1000000) >> send "Hello World!" >> p
  in  concurrently_ c p
@

We have a delay of 3 seconds before publishing to make sure the consumer is already running. Otherwise, it might miss some messages.

Finally, we put it all together and call 'runPulsar' with the connection and the program in the 'Pulsar' monad.

@
main :: IO ()
main = runPulsar conn pulsar
@

Since a Pulsar connection, consumers, and producers are long-lived resources, Supernova manages them accordingly for you. Once the program exits, the resources will be released in the respective order (always opposite to the order of acquisition).
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
