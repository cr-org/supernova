{-|
Module      : Pulsar
Description : Apache Pulsar client
License     : Apache-2.0
Maintainer  : gabriel.volpe@chatroulette.com
Stability   : experimental

Apache Pulsar client that implements the required TCP protocol. Acquire a connection and create as many consumers and producers as you need.
-}
module Pulsar
  ( ConnectData
  , Consumer(..)
  , LogLevel(..)
  , LogOptions(..)
  , LogOutput(..)
  , Producer(..)
  , Pulsar
  , PulsarCtx
  , connect
  , defaultConnectData
  , newConsumer
  , newProducer
  , runPulsar
  , runPulsar'
  , module Pulsar.Types
  )
where

import           Pulsar.Connection
import           Pulsar.Consumer
import           Pulsar.Core             hiding ( newProducer )
import           Pulsar.Internal.Core
import           Pulsar.Producer
import           Pulsar.Types
