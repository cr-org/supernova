{-# LANGUAGE OverloadedStrings #-}

module Pulsar.Protocol.Commands where

import qualified Data.Binary                   as B
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

subscribe :: B.Word64 -> Topic -> SubscriptionName -> BaseCommand
subscribe cid topic (SubscriptionName sub) = defMessage
    & F.type' .~ BaseCommand'SUBSCRIBE
    & F.subscribe .~ subs
 where
  subs :: CommandSubscribe
  subs = defMessage
    & F.topic .~ T.pack (show topic)
    & F.subscription .~ sub
    & F.subType .~ CommandSubscribe'Shared
    & F.consumerId .~ cid

flow :: B.Word64 -> BaseCommand
flow cid = defMessage
    & F.type' .~ BaseCommand'FLOW
    & F.flow .~ flowCmd
 where
  flowCmd :: CommandFlow
  flowCmd = defMessage
    & F.messagePermits .~ 100
    & F.consumerId .~ cid

ack :: B.Word64 -> MessageIdData -> BaseCommand
ack cid msgId = defMessage
    & F.type' .~ BaseCommand'ACK
    & F.ack .~ ackCmd
 where
  ackCmd :: CommandAck
  ackCmd = defMessage
    & F.messageId .~ [ msgId ]
    & F.consumerId .~ cid

closeConsumer :: B.Word64 -> BaseCommand
closeConsumer cid = defMessage
    & F.type' .~ BaseCommand'CLOSE_CONSUMER
    & F.closeConsumer .~ close
 where
  close :: CommandCloseConsumer
  close = defMessage
    & F.consumerId .~ cid

producer :: B.Word64 -> Topic -> BaseCommand
producer pid topic = defMessage
    & F.type' .~ BaseCommand'PRODUCER
    & F.producer .~ prod
 where
  prod :: CommandProducer
  prod = defMessage
    & F.topic .~ T.pack (show topic)
    & F.producerId .~ pid

closeProducer :: B.Word64 -> BaseCommand
closeProducer pid = defMessage
    & F.type' .~ BaseCommand'CLOSE_PRODUCER
    & F.closeProducer .~ prod
 where
  prod :: CommandCloseProducer
  prod = defMessage
    & F.producerId .~ pid

send :: B.Word64 -> B.Word64 -> BaseCommand
send pid sid = defMessage
    & F.type' .~ BaseCommand'SEND
    & F.send .~ sendCmd
 where
  sendCmd :: CommandSend
  sendCmd = defMessage
    & F.numMessages .~ 1
    & F.producerId .~ pid
    & F.sequenceId .~ sid

lookup :: Topic -> BaseCommand
lookup topic = defMessage
    & F.type' .~ BaseCommand'LOOKUP
    & F.lookupTopic .~ lut
  where
   lut :: CommandLookupTopic
   lut = defMessage
     & F.topic .~ T.pack (show topic)

------- Keep Alive --------

ping :: BaseCommand
ping = defMessage
    & F.type' .~ BaseCommand'PING
    & F.ping .~ defMessage

pong :: BaseCommand
pong = defMessage
    & F.type' .~ BaseCommand'PONG
    & F.pong .~ defMessage

------- Metadata for Payload commands --------

messageMetadata :: MessageMetadata
messageMetadata = defMessage
