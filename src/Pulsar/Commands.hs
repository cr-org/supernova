{-# LANGUAGE OverloadedStrings #-}

module Pulsar.Commands where

import           Data.ProtoLens                 ( defMessage )
import qualified Data.Text                     as T
import           Data.Version                   ( showVersion )
import           Lens.Family
import           Paths_hpulsar                  ( version )
import           Proto.PulsarApi
import qualified Proto.PulsarApi_Fields        as F
import Pulsar.Data

cmdConnect :: BaseCommand
cmdConnect = defMessage
    & F.connect .~ connect
 where
  connect :: CommandConnect
  connect = defMessage
    & F.clientVersion .~ "Pulsar-Client-Haskell-v" <> T.pack (showVersion version)
    & F.protocolVersion .~ 15

cmdSubscribe :: Topic -> SubscriptionName -> BaseCommand
cmdSubscribe topic (SubscriptionName sub) = defMessage
    & F.type' .~ BaseCommand'SUBSCRIBE
    & F.subscribe .~ subscribe
 where
  subscribe :: CommandSubscribe
  subscribe = defMessage
    & F.topic .~ T.pack (show topic)
    & F.subscription .~ sub
    & F.subType .~ CommandSubscribe'Shared

cmdProducer :: Topic -> BaseCommand
cmdProducer topic = defMessage
    & F.type' .~ BaseCommand'PRODUCER
    & F.producer .~ producer
 where
  producer :: CommandProducer
  producer = defMessage
    & F.topic .~ T.pack (show topic)
    & F.producerId .~ 0
    & F.requestId .~ 0

cmdPing :: BaseCommand
cmdPing = defMessage
    & F.type' .~ BaseCommand'PING
    & F.ping .~ (defMessage :: CommandPing)
