{-# LANGUAGE OverloadedStrings #-}

module Pulsar.Protocol.Commands where

import           Data.ProtoLens                 ( defMessage )
import qualified Data.Text                     as T
import           Data.Version                   ( showVersion )
import           Lens.Family
import           Paths_hpulsar                  ( version )
import           Proto.PulsarApi
import qualified Proto.PulsarApi_Fields        as F
import           Pulsar.Types

connect :: BaseCommand
connect = defMessage
    & F.connect .~ conn
 where
  conn :: CommandConnect
  conn = defMessage
    & F.clientVersion .~ "Pulsar-Client-Haskell-v" <> T.pack (showVersion version)
    & F.protocolVersion .~ 15

subscribe :: Topic -> SubscriptionName -> BaseCommand
subscribe topic (SubscriptionName sub) = defMessage
    & F.type' .~ BaseCommand'SUBSCRIBE
    & F.subscribe .~ subs
 where
  subs :: CommandSubscribe
  subs = defMessage
    & F.topic .~ T.pack (show topic)
    & F.subscription .~ sub
    & F.subType .~ CommandSubscribe'Shared

producer :: Topic -> BaseCommand
producer topic = defMessage
    & F.type' .~ BaseCommand'PRODUCER
    & F.producer .~ prod
 where
  prod :: CommandProducer
  prod = defMessage
    & F.topic .~ T.pack (show topic)
    & F.producerId .~ 0
    & F.requestId .~ 0

closeProducer :: BaseCommand
closeProducer = defMessage
    & F.type' .~ BaseCommand'CLOSE_PRODUCER
    & F.closeProducer .~ prod
 where
  prod :: CommandCloseProducer
  prod = defMessage
    & F.producerId .~ 0
    & F.requestId .~ 0

send :: BaseCommand
send = defMessage
    & F.type' .~ BaseCommand'SEND
    & F.send .~ sendCmd
 where
  sendCmd :: CommandSend
  sendCmd = defMessage
    & F.numMessages .~ 1

ping :: BaseCommand
ping = defMessage
    & F.type' .~ BaseCommand'PING
    & F.ping .~ defMessage

lookup :: Topic -> BaseCommand
lookup topic = defMessage
    & F.type' .~ BaseCommand'LOOKUP
    & F.lookupTopic .~ lut
  where
   lut :: CommandLookupTopic
   lut = defMessage
     & F.topic .~ T.pack (show topic)

------- Metadata for Payload commands --------

singleMessageMetadata :: SingleMessageMetadata
singleMessageMetadata = defMessage
