{- This file was auto-generated from pulsar_api.proto by the proto-lens-protoc program. -}
{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeFamilies, UndecidableInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, PatternSynonyms, MagicHash, NoImplicitPrelude, DataKinds, BangPatterns, TypeApplications, OverloadedStrings, DerivingStrategies#-}
{-# OPTIONS_GHC -Wno-unused-imports#-}
{-# OPTIONS_GHC -Wno-duplicate-exports#-}
{-# OPTIONS_GHC -Wno-dodgy-exports#-}
module Proto.PulsarApi_Fields where
import qualified Data.ProtoLens.Runtime.Prelude as Prelude
import qualified Data.ProtoLens.Runtime.Data.Int as Data.Int
import qualified Data.ProtoLens.Runtime.Data.Monoid as Data.Monoid
import qualified Data.ProtoLens.Runtime.Data.Word as Data.Word
import qualified Data.ProtoLens.Runtime.Data.ProtoLens as Data.ProtoLens
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Encoding.Bytes as Data.ProtoLens.Encoding.Bytes
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Encoding.Growing as Data.ProtoLens.Encoding.Growing
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Encoding.Parser.Unsafe as Data.ProtoLens.Encoding.Parser.Unsafe
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Encoding.Wire as Data.ProtoLens.Encoding.Wire
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Field as Data.ProtoLens.Field
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Message.Enum as Data.ProtoLens.Message.Enum
import qualified Data.ProtoLens.Runtime.Data.ProtoLens.Service.Types as Data.ProtoLens.Service.Types
import qualified Data.ProtoLens.Runtime.Lens.Family2 as Lens.Family2
import qualified Data.ProtoLens.Runtime.Lens.Family2.Unchecked as Lens.Family2.Unchecked
import qualified Data.ProtoLens.Runtime.Data.Text as Data.Text
import qualified Data.ProtoLens.Runtime.Data.Map as Data.Map
import qualified Data.ProtoLens.Runtime.Data.ByteString as Data.ByteString
import qualified Data.ProtoLens.Runtime.Data.ByteString.Char8 as Data.ByteString.Char8
import qualified Data.ProtoLens.Runtime.Data.Text.Encoding as Data.Text.Encoding
import qualified Data.ProtoLens.Runtime.Data.Vector as Data.Vector
import qualified Data.ProtoLens.Runtime.Data.Vector.Generic as Data.Vector.Generic
import qualified Data.ProtoLens.Runtime.Data.Vector.Unboxed as Data.Vector.Unboxed
import qualified Data.ProtoLens.Runtime.Text.Read as Text.Read
ack ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "ack" a) =>
  Lens.Family2.LensLike' f s a
ack = Data.ProtoLens.Field.field @"ack"
ackResponse ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "ackResponse" a) =>
  Lens.Family2.LensLike' f s a
ackResponse = Data.ProtoLens.Field.field @"ackResponse"
ackSet ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "ackSet" a) =>
  Lens.Family2.LensLike' f s a
ackSet = Data.ProtoLens.Field.field @"ackSet"
ackType ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "ackType" a) =>
  Lens.Family2.LensLike' f s a
ackType = Data.ProtoLens.Field.field @"ackType"
activeConsumerChange ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "activeConsumerChange" a) =>
  Lens.Family2.LensLike' f s a
activeConsumerChange
  = Data.ProtoLens.Field.field @"activeConsumerChange"
addPartitionToTxn ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "addPartitionToTxn" a) =>
  Lens.Family2.LensLike' f s a
addPartitionToTxn = Data.ProtoLens.Field.field @"addPartitionToTxn"
addPartitionToTxnResponse ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "addPartitionToTxnResponse" a) =>
  Lens.Family2.LensLike' f s a
addPartitionToTxnResponse
  = Data.ProtoLens.Field.field @"addPartitionToTxnResponse"
addSubscriptionToTxn ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "addSubscriptionToTxn" a) =>
  Lens.Family2.LensLike' f s a
addSubscriptionToTxn
  = Data.ProtoLens.Field.field @"addSubscriptionToTxn"
addSubscriptionToTxnResponse ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "addSubscriptionToTxnResponse" a) =>
  Lens.Family2.LensLike' f s a
addSubscriptionToTxnResponse
  = Data.ProtoLens.Field.field @"addSubscriptionToTxnResponse"
address ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "address" a) =>
  Lens.Family2.LensLike' f s a
address = Data.ProtoLens.Field.field @"address"
advertisedListenerName ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "advertisedListenerName" a) =>
  Lens.Family2.LensLike' f s a
advertisedListenerName
  = Data.ProtoLens.Field.field @"advertisedListenerName"
allowOutOfOrderDelivery ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "allowOutOfOrderDelivery" a) =>
  Lens.Family2.LensLike' f s a
allowOutOfOrderDelivery
  = Data.ProtoLens.Field.field @"allowOutOfOrderDelivery"
authChallenge ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "authChallenge" a) =>
  Lens.Family2.LensLike' f s a
authChallenge = Data.ProtoLens.Field.field @"authChallenge"
authData ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "authData" a) =>
  Lens.Family2.LensLike' f s a
authData = Data.ProtoLens.Field.field @"authData"
authMethod ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "authMethod" a) =>
  Lens.Family2.LensLike' f s a
authMethod = Data.ProtoLens.Field.field @"authMethod"
authMethodName ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "authMethodName" a) =>
  Lens.Family2.LensLike' f s a
authMethodName = Data.ProtoLens.Field.field @"authMethodName"
authResponse ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "authResponse" a) =>
  Lens.Family2.LensLike' f s a
authResponse = Data.ProtoLens.Field.field @"authResponse"
authoritative ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "authoritative" a) =>
  Lens.Family2.LensLike' f s a
authoritative = Data.ProtoLens.Field.field @"authoritative"
availablePermits ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "availablePermits" a) =>
  Lens.Family2.LensLike' f s a
availablePermits = Data.ProtoLens.Field.field @"availablePermits"
batchIndex ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "batchIndex" a) =>
  Lens.Family2.LensLike' f s a
batchIndex = Data.ProtoLens.Field.field @"batchIndex"
blockedConsumerOnUnackedMsgs ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "blockedConsumerOnUnackedMsgs" a) =>
  Lens.Family2.LensLike' f s a
blockedConsumerOnUnackedMsgs
  = Data.ProtoLens.Field.field @"blockedConsumerOnUnackedMsgs"
brokerServiceUrl ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "brokerServiceUrl" a) =>
  Lens.Family2.LensLike' f s a
brokerServiceUrl = Data.ProtoLens.Field.field @"brokerServiceUrl"
brokerServiceUrlTls ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "brokerServiceUrlTls" a) =>
  Lens.Family2.LensLike' f s a
brokerServiceUrlTls
  = Data.ProtoLens.Field.field @"brokerServiceUrlTls"
challenge ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "challenge" a) =>
  Lens.Family2.LensLike' f s a
challenge = Data.ProtoLens.Field.field @"challenge"
chunkId ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "chunkId" a) =>
  Lens.Family2.LensLike' f s a
chunkId = Data.ProtoLens.Field.field @"chunkId"
clientVersion ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "clientVersion" a) =>
  Lens.Family2.LensLike' f s a
clientVersion = Data.ProtoLens.Field.field @"clientVersion"
closeConsumer ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "closeConsumer" a) =>
  Lens.Family2.LensLike' f s a
closeConsumer = Data.ProtoLens.Field.field @"closeConsumer"
closeProducer ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "closeProducer" a) =>
  Lens.Family2.LensLike' f s a
closeProducer = Data.ProtoLens.Field.field @"closeProducer"
compactedOut ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "compactedOut" a) =>
  Lens.Family2.LensLike' f s a
compactedOut = Data.ProtoLens.Field.field @"compactedOut"
compression ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "compression" a) =>
  Lens.Family2.LensLike' f s a
compression = Data.ProtoLens.Field.field @"compression"
connect ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "connect" a) =>
  Lens.Family2.LensLike' f s a
connect = Data.ProtoLens.Field.field @"connect"
connected ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "connected" a) =>
  Lens.Family2.LensLike' f s a
connected = Data.ProtoLens.Field.field @"connected"
connectedSince ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "connectedSince" a) =>
  Lens.Family2.LensLike' f s a
connectedSince = Data.ProtoLens.Field.field @"connectedSince"
consumerId ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "consumerId" a) =>
  Lens.Family2.LensLike' f s a
consumerId = Data.ProtoLens.Field.field @"consumerId"
consumerName ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "consumerName" a) =>
  Lens.Family2.LensLike' f s a
consumerName = Data.ProtoLens.Field.field @"consumerName"
consumerStats ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "consumerStats" a) =>
  Lens.Family2.LensLike' f s a
consumerStats = Data.ProtoLens.Field.field @"consumerStats"
consumerStatsResponse ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "consumerStatsResponse" a) =>
  Lens.Family2.LensLike' f s a
consumerStatsResponse
  = Data.ProtoLens.Field.field @"consumerStatsResponse"
deliverAtTime ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "deliverAtTime" a) =>
  Lens.Family2.LensLike' f s a
deliverAtTime = Data.ProtoLens.Field.field @"deliverAtTime"
durable ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "durable" a) =>
  Lens.Family2.LensLike' f s a
durable = Data.ProtoLens.Field.field @"durable"
encrypted ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "encrypted" a) =>
  Lens.Family2.LensLike' f s a
encrypted = Data.ProtoLens.Field.field @"encrypted"
encryptionAlgo ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "encryptionAlgo" a) =>
  Lens.Family2.LensLike' f s a
encryptionAlgo = Data.ProtoLens.Field.field @"encryptionAlgo"
encryptionKeys ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "encryptionKeys" a) =>
  Lens.Family2.LensLike' f s a
encryptionKeys = Data.ProtoLens.Field.field @"encryptionKeys"
encryptionParam ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "encryptionParam" a) =>
  Lens.Family2.LensLike' f s a
encryptionParam = Data.ProtoLens.Field.field @"encryptionParam"
end ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "end" a) =>
  Lens.Family2.LensLike' f s a
end = Data.ProtoLens.Field.field @"end"
endTxn ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "endTxn" a) =>
  Lens.Family2.LensLike' f s a
endTxn = Data.ProtoLens.Field.field @"endTxn"
endTxnOnPartition ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "endTxnOnPartition" a) =>
  Lens.Family2.LensLike' f s a
endTxnOnPartition = Data.ProtoLens.Field.field @"endTxnOnPartition"
endTxnOnPartitionResponse ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "endTxnOnPartitionResponse" a) =>
  Lens.Family2.LensLike' f s a
endTxnOnPartitionResponse
  = Data.ProtoLens.Field.field @"endTxnOnPartitionResponse"
endTxnOnSubscription ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "endTxnOnSubscription" a) =>
  Lens.Family2.LensLike' f s a
endTxnOnSubscription
  = Data.ProtoLens.Field.field @"endTxnOnSubscription"
endTxnOnSubscriptionResponse ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "endTxnOnSubscriptionResponse" a) =>
  Lens.Family2.LensLike' f s a
endTxnOnSubscriptionResponse
  = Data.ProtoLens.Field.field @"endTxnOnSubscriptionResponse"
endTxnResponse ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "endTxnResponse" a) =>
  Lens.Family2.LensLike' f s a
endTxnResponse = Data.ProtoLens.Field.field @"endTxnResponse"
entryId ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "entryId" a) =>
  Lens.Family2.LensLike' f s a
entryId = Data.ProtoLens.Field.field @"entryId"
epoch ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "epoch" a) =>
  Lens.Family2.LensLike' f s a
epoch = Data.ProtoLens.Field.field @"epoch"
error ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "error" a) =>
  Lens.Family2.LensLike' f s a
error = Data.ProtoLens.Field.field @"error"
errorCode ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "errorCode" a) =>
  Lens.Family2.LensLike' f s a
errorCode = Data.ProtoLens.Field.field @"errorCode"
errorMessage ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "errorMessage" a) =>
  Lens.Family2.LensLike' f s a
errorMessage = Data.ProtoLens.Field.field @"errorMessage"
eventTime ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "eventTime" a) =>
  Lens.Family2.LensLike' f s a
eventTime = Data.ProtoLens.Field.field @"eventTime"
featureFlags ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "featureFlags" a) =>
  Lens.Family2.LensLike' f s a
featureFlags = Data.ProtoLens.Field.field @"featureFlags"
flow ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "flow" a) =>
  Lens.Family2.LensLike' f s a
flow = Data.ProtoLens.Field.field @"flow"
forceTopicCreation ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "forceTopicCreation" a) =>
  Lens.Family2.LensLike' f s a
forceTopicCreation
  = Data.ProtoLens.Field.field @"forceTopicCreation"
getLastMessageId ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "getLastMessageId" a) =>
  Lens.Family2.LensLike' f s a
getLastMessageId = Data.ProtoLens.Field.field @"getLastMessageId"
getLastMessageIdResponse ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "getLastMessageIdResponse" a) =>
  Lens.Family2.LensLike' f s a
getLastMessageIdResponse
  = Data.ProtoLens.Field.field @"getLastMessageIdResponse"
getOrCreateSchema ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "getOrCreateSchema" a) =>
  Lens.Family2.LensLike' f s a
getOrCreateSchema = Data.ProtoLens.Field.field @"getOrCreateSchema"
getOrCreateSchemaResponse ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "getOrCreateSchemaResponse" a) =>
  Lens.Family2.LensLike' f s a
getOrCreateSchemaResponse
  = Data.ProtoLens.Field.field @"getOrCreateSchemaResponse"
getSchema ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "getSchema" a) =>
  Lens.Family2.LensLike' f s a
getSchema = Data.ProtoLens.Field.field @"getSchema"
getSchemaResponse ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "getSchemaResponse" a) =>
  Lens.Family2.LensLike' f s a
getSchemaResponse = Data.ProtoLens.Field.field @"getSchemaResponse"
getTopicsOfNamespace ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "getTopicsOfNamespace" a) =>
  Lens.Family2.LensLike' f s a
getTopicsOfNamespace
  = Data.ProtoLens.Field.field @"getTopicsOfNamespace"
getTopicsOfNamespaceResponse ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "getTopicsOfNamespaceResponse" a) =>
  Lens.Family2.LensLike' f s a
getTopicsOfNamespaceResponse
  = Data.ProtoLens.Field.field @"getTopicsOfNamespaceResponse"
hashRanges ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "hashRanges" a) =>
  Lens.Family2.LensLike' f s a
hashRanges = Data.ProtoLens.Field.field @"hashRanges"
highestSequenceId ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "highestSequenceId" a) =>
  Lens.Family2.LensLike' f s a
highestSequenceId = Data.ProtoLens.Field.field @"highestSequenceId"
initialPosition ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "initialPosition" a) =>
  Lens.Family2.LensLike' f s a
initialPosition = Data.ProtoLens.Field.field @"initialPosition"
isActive ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "isActive" a) =>
  Lens.Family2.LensLike' f s a
isActive = Data.ProtoLens.Field.field @"isActive"
isChunk ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "isChunk" a) =>
  Lens.Family2.LensLike' f s a
isChunk = Data.ProtoLens.Field.field @"isChunk"
key ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "key" a) =>
  Lens.Family2.LensLike' f s a
key = Data.ProtoLens.Field.field @"key"
keySharedMeta ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "keySharedMeta" a) =>
  Lens.Family2.LensLike' f s a
keySharedMeta = Data.ProtoLens.Field.field @"keySharedMeta"
keySharedMode ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "keySharedMode" a) =>
  Lens.Family2.LensLike' f s a
keySharedMode = Data.ProtoLens.Field.field @"keySharedMode"
lastMessageId ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "lastMessageId" a) =>
  Lens.Family2.LensLike' f s a
lastMessageId = Data.ProtoLens.Field.field @"lastMessageId"
lastSequenceId ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "lastSequenceId" a) =>
  Lens.Family2.LensLike' f s a
lastSequenceId = Data.ProtoLens.Field.field @"lastSequenceId"
ledgerId ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "ledgerId" a) =>
  Lens.Family2.LensLike' f s a
ledgerId = Data.ProtoLens.Field.field @"ledgerId"
lookupTopic ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "lookupTopic" a) =>
  Lens.Family2.LensLike' f s a
lookupTopic = Data.ProtoLens.Field.field @"lookupTopic"
lookupTopicResponse ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "lookupTopicResponse" a) =>
  Lens.Family2.LensLike' f s a
lookupTopicResponse
  = Data.ProtoLens.Field.field @"lookupTopicResponse"
markerType ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "markerType" a) =>
  Lens.Family2.LensLike' f s a
markerType = Data.ProtoLens.Field.field @"markerType"
maxMessageSize ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maxMessageSize" a) =>
  Lens.Family2.LensLike' f s a
maxMessageSize = Data.ProtoLens.Field.field @"maxMessageSize"
maybe'ack ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'ack" a) =>
  Lens.Family2.LensLike' f s a
maybe'ack = Data.ProtoLens.Field.field @"maybe'ack"
maybe'ackResponse ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'ackResponse" a) =>
  Lens.Family2.LensLike' f s a
maybe'ackResponse = Data.ProtoLens.Field.field @"maybe'ackResponse"
maybe'activeConsumerChange ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'activeConsumerChange" a) =>
  Lens.Family2.LensLike' f s a
maybe'activeConsumerChange
  = Data.ProtoLens.Field.field @"maybe'activeConsumerChange"
maybe'addPartitionToTxn ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'addPartitionToTxn" a) =>
  Lens.Family2.LensLike' f s a
maybe'addPartitionToTxn
  = Data.ProtoLens.Field.field @"maybe'addPartitionToTxn"
maybe'addPartitionToTxnResponse ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'addPartitionToTxnResponse" a) =>
  Lens.Family2.LensLike' f s a
maybe'addPartitionToTxnResponse
  = Data.ProtoLens.Field.field @"maybe'addPartitionToTxnResponse"
maybe'addSubscriptionToTxn ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'addSubscriptionToTxn" a) =>
  Lens.Family2.LensLike' f s a
maybe'addSubscriptionToTxn
  = Data.ProtoLens.Field.field @"maybe'addSubscriptionToTxn"
maybe'addSubscriptionToTxnResponse ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'addSubscriptionToTxnResponse" a) =>
  Lens.Family2.LensLike' f s a
maybe'addSubscriptionToTxnResponse
  = Data.ProtoLens.Field.field @"maybe'addSubscriptionToTxnResponse"
maybe'address ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'address" a) =>
  Lens.Family2.LensLike' f s a
maybe'address = Data.ProtoLens.Field.field @"maybe'address"
maybe'advertisedListenerName ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'advertisedListenerName" a) =>
  Lens.Family2.LensLike' f s a
maybe'advertisedListenerName
  = Data.ProtoLens.Field.field @"maybe'advertisedListenerName"
maybe'allowOutOfOrderDelivery ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'allowOutOfOrderDelivery" a) =>
  Lens.Family2.LensLike' f s a
maybe'allowOutOfOrderDelivery
  = Data.ProtoLens.Field.field @"maybe'allowOutOfOrderDelivery"
maybe'authChallenge ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'authChallenge" a) =>
  Lens.Family2.LensLike' f s a
maybe'authChallenge
  = Data.ProtoLens.Field.field @"maybe'authChallenge"
maybe'authData ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'authData" a) =>
  Lens.Family2.LensLike' f s a
maybe'authData = Data.ProtoLens.Field.field @"maybe'authData"
maybe'authMethod ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'authMethod" a) =>
  Lens.Family2.LensLike' f s a
maybe'authMethod = Data.ProtoLens.Field.field @"maybe'authMethod"
maybe'authMethodName ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'authMethodName" a) =>
  Lens.Family2.LensLike' f s a
maybe'authMethodName
  = Data.ProtoLens.Field.field @"maybe'authMethodName"
maybe'authResponse ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'authResponse" a) =>
  Lens.Family2.LensLike' f s a
maybe'authResponse
  = Data.ProtoLens.Field.field @"maybe'authResponse"
maybe'authoritative ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'authoritative" a) =>
  Lens.Family2.LensLike' f s a
maybe'authoritative
  = Data.ProtoLens.Field.field @"maybe'authoritative"
maybe'availablePermits ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'availablePermits" a) =>
  Lens.Family2.LensLike' f s a
maybe'availablePermits
  = Data.ProtoLens.Field.field @"maybe'availablePermits"
maybe'batchIndex ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'batchIndex" a) =>
  Lens.Family2.LensLike' f s a
maybe'batchIndex = Data.ProtoLens.Field.field @"maybe'batchIndex"
maybe'blockedConsumerOnUnackedMsgs ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'blockedConsumerOnUnackedMsgs" a) =>
  Lens.Family2.LensLike' f s a
maybe'blockedConsumerOnUnackedMsgs
  = Data.ProtoLens.Field.field @"maybe'blockedConsumerOnUnackedMsgs"
maybe'brokerServiceUrl ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'brokerServiceUrl" a) =>
  Lens.Family2.LensLike' f s a
maybe'brokerServiceUrl
  = Data.ProtoLens.Field.field @"maybe'brokerServiceUrl"
maybe'brokerServiceUrlTls ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'brokerServiceUrlTls" a) =>
  Lens.Family2.LensLike' f s a
maybe'brokerServiceUrlTls
  = Data.ProtoLens.Field.field @"maybe'brokerServiceUrlTls"
maybe'challenge ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'challenge" a) =>
  Lens.Family2.LensLike' f s a
maybe'challenge = Data.ProtoLens.Field.field @"maybe'challenge"
maybe'chunkId ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'chunkId" a) =>
  Lens.Family2.LensLike' f s a
maybe'chunkId = Data.ProtoLens.Field.field @"maybe'chunkId"
maybe'clientVersion ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'clientVersion" a) =>
  Lens.Family2.LensLike' f s a
maybe'clientVersion
  = Data.ProtoLens.Field.field @"maybe'clientVersion"
maybe'closeConsumer ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'closeConsumer" a) =>
  Lens.Family2.LensLike' f s a
maybe'closeConsumer
  = Data.ProtoLens.Field.field @"maybe'closeConsumer"
maybe'closeProducer ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'closeProducer" a) =>
  Lens.Family2.LensLike' f s a
maybe'closeProducer
  = Data.ProtoLens.Field.field @"maybe'closeProducer"
maybe'compactedOut ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'compactedOut" a) =>
  Lens.Family2.LensLike' f s a
maybe'compactedOut
  = Data.ProtoLens.Field.field @"maybe'compactedOut"
maybe'compression ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'compression" a) =>
  Lens.Family2.LensLike' f s a
maybe'compression = Data.ProtoLens.Field.field @"maybe'compression"
maybe'connect ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'connect" a) =>
  Lens.Family2.LensLike' f s a
maybe'connect = Data.ProtoLens.Field.field @"maybe'connect"
maybe'connected ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'connected" a) =>
  Lens.Family2.LensLike' f s a
maybe'connected = Data.ProtoLens.Field.field @"maybe'connected"
maybe'connectedSince ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'connectedSince" a) =>
  Lens.Family2.LensLike' f s a
maybe'connectedSince
  = Data.ProtoLens.Field.field @"maybe'connectedSince"
maybe'consumerName ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'consumerName" a) =>
  Lens.Family2.LensLike' f s a
maybe'consumerName
  = Data.ProtoLens.Field.field @"maybe'consumerName"
maybe'consumerStats ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'consumerStats" a) =>
  Lens.Family2.LensLike' f s a
maybe'consumerStats
  = Data.ProtoLens.Field.field @"maybe'consumerStats"
maybe'consumerStatsResponse ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'consumerStatsResponse" a) =>
  Lens.Family2.LensLike' f s a
maybe'consumerStatsResponse
  = Data.ProtoLens.Field.field @"maybe'consumerStatsResponse"
maybe'deliverAtTime ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'deliverAtTime" a) =>
  Lens.Family2.LensLike' f s a
maybe'deliverAtTime
  = Data.ProtoLens.Field.field @"maybe'deliverAtTime"
maybe'durable ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'durable" a) =>
  Lens.Family2.LensLike' f s a
maybe'durable = Data.ProtoLens.Field.field @"maybe'durable"
maybe'encrypted ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'encrypted" a) =>
  Lens.Family2.LensLike' f s a
maybe'encrypted = Data.ProtoLens.Field.field @"maybe'encrypted"
maybe'encryptionAlgo ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'encryptionAlgo" a) =>
  Lens.Family2.LensLike' f s a
maybe'encryptionAlgo
  = Data.ProtoLens.Field.field @"maybe'encryptionAlgo"
maybe'encryptionParam ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'encryptionParam" a) =>
  Lens.Family2.LensLike' f s a
maybe'encryptionParam
  = Data.ProtoLens.Field.field @"maybe'encryptionParam"
maybe'endTxn ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'endTxn" a) =>
  Lens.Family2.LensLike' f s a
maybe'endTxn = Data.ProtoLens.Field.field @"maybe'endTxn"
maybe'endTxnOnPartition ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'endTxnOnPartition" a) =>
  Lens.Family2.LensLike' f s a
maybe'endTxnOnPartition
  = Data.ProtoLens.Field.field @"maybe'endTxnOnPartition"
maybe'endTxnOnPartitionResponse ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'endTxnOnPartitionResponse" a) =>
  Lens.Family2.LensLike' f s a
maybe'endTxnOnPartitionResponse
  = Data.ProtoLens.Field.field @"maybe'endTxnOnPartitionResponse"
maybe'endTxnOnSubscription ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'endTxnOnSubscription" a) =>
  Lens.Family2.LensLike' f s a
maybe'endTxnOnSubscription
  = Data.ProtoLens.Field.field @"maybe'endTxnOnSubscription"
maybe'endTxnOnSubscriptionResponse ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'endTxnOnSubscriptionResponse" a) =>
  Lens.Family2.LensLike' f s a
maybe'endTxnOnSubscriptionResponse
  = Data.ProtoLens.Field.field @"maybe'endTxnOnSubscriptionResponse"
maybe'endTxnResponse ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'endTxnResponse" a) =>
  Lens.Family2.LensLike' f s a
maybe'endTxnResponse
  = Data.ProtoLens.Field.field @"maybe'endTxnResponse"
maybe'epoch ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'epoch" a) =>
  Lens.Family2.LensLike' f s a
maybe'epoch = Data.ProtoLens.Field.field @"maybe'epoch"
maybe'error ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'error" a) =>
  Lens.Family2.LensLike' f s a
maybe'error = Data.ProtoLens.Field.field @"maybe'error"
maybe'errorCode ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'errorCode" a) =>
  Lens.Family2.LensLike' f s a
maybe'errorCode = Data.ProtoLens.Field.field @"maybe'errorCode"
maybe'errorMessage ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'errorMessage" a) =>
  Lens.Family2.LensLike' f s a
maybe'errorMessage
  = Data.ProtoLens.Field.field @"maybe'errorMessage"
maybe'eventTime ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'eventTime" a) =>
  Lens.Family2.LensLike' f s a
maybe'eventTime = Data.ProtoLens.Field.field @"maybe'eventTime"
maybe'featureFlags ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'featureFlags" a) =>
  Lens.Family2.LensLike' f s a
maybe'featureFlags
  = Data.ProtoLens.Field.field @"maybe'featureFlags"
maybe'flow ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'flow" a) =>
  Lens.Family2.LensLike' f s a
maybe'flow = Data.ProtoLens.Field.field @"maybe'flow"
maybe'forceTopicCreation ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'forceTopicCreation" a) =>
  Lens.Family2.LensLike' f s a
maybe'forceTopicCreation
  = Data.ProtoLens.Field.field @"maybe'forceTopicCreation"
maybe'getLastMessageId ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'getLastMessageId" a) =>
  Lens.Family2.LensLike' f s a
maybe'getLastMessageId
  = Data.ProtoLens.Field.field @"maybe'getLastMessageId"
maybe'getLastMessageIdResponse ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'getLastMessageIdResponse" a) =>
  Lens.Family2.LensLike' f s a
maybe'getLastMessageIdResponse
  = Data.ProtoLens.Field.field @"maybe'getLastMessageIdResponse"
maybe'getOrCreateSchema ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'getOrCreateSchema" a) =>
  Lens.Family2.LensLike' f s a
maybe'getOrCreateSchema
  = Data.ProtoLens.Field.field @"maybe'getOrCreateSchema"
maybe'getOrCreateSchemaResponse ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'getOrCreateSchemaResponse" a) =>
  Lens.Family2.LensLike' f s a
maybe'getOrCreateSchemaResponse
  = Data.ProtoLens.Field.field @"maybe'getOrCreateSchemaResponse"
maybe'getSchema ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'getSchema" a) =>
  Lens.Family2.LensLike' f s a
maybe'getSchema = Data.ProtoLens.Field.field @"maybe'getSchema"
maybe'getSchemaResponse ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'getSchemaResponse" a) =>
  Lens.Family2.LensLike' f s a
maybe'getSchemaResponse
  = Data.ProtoLens.Field.field @"maybe'getSchemaResponse"
maybe'getTopicsOfNamespace ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'getTopicsOfNamespace" a) =>
  Lens.Family2.LensLike' f s a
maybe'getTopicsOfNamespace
  = Data.ProtoLens.Field.field @"maybe'getTopicsOfNamespace"
maybe'getTopicsOfNamespaceResponse ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'getTopicsOfNamespaceResponse" a) =>
  Lens.Family2.LensLike' f s a
maybe'getTopicsOfNamespaceResponse
  = Data.ProtoLens.Field.field @"maybe'getTopicsOfNamespaceResponse"
maybe'highestSequenceId ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'highestSequenceId" a) =>
  Lens.Family2.LensLike' f s a
maybe'highestSequenceId
  = Data.ProtoLens.Field.field @"maybe'highestSequenceId"
maybe'initialPosition ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'initialPosition" a) =>
  Lens.Family2.LensLike' f s a
maybe'initialPosition
  = Data.ProtoLens.Field.field @"maybe'initialPosition"
maybe'isActive ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'isActive" a) =>
  Lens.Family2.LensLike' f s a
maybe'isActive = Data.ProtoLens.Field.field @"maybe'isActive"
maybe'isChunk ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'isChunk" a) =>
  Lens.Family2.LensLike' f s a
maybe'isChunk = Data.ProtoLens.Field.field @"maybe'isChunk"
maybe'keySharedMeta ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'keySharedMeta" a) =>
  Lens.Family2.LensLike' f s a
maybe'keySharedMeta
  = Data.ProtoLens.Field.field @"maybe'keySharedMeta"
maybe'lastSequenceId ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'lastSequenceId" a) =>
  Lens.Family2.LensLike' f s a
maybe'lastSequenceId
  = Data.ProtoLens.Field.field @"maybe'lastSequenceId"
maybe'lookupTopic ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'lookupTopic" a) =>
  Lens.Family2.LensLike' f s a
maybe'lookupTopic = Data.ProtoLens.Field.field @"maybe'lookupTopic"
maybe'lookupTopicResponse ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'lookupTopicResponse" a) =>
  Lens.Family2.LensLike' f s a
maybe'lookupTopicResponse
  = Data.ProtoLens.Field.field @"maybe'lookupTopicResponse"
maybe'markerType ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'markerType" a) =>
  Lens.Family2.LensLike' f s a
maybe'markerType = Data.ProtoLens.Field.field @"maybe'markerType"
maybe'maxMessageSize ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'maxMessageSize" a) =>
  Lens.Family2.LensLike' f s a
maybe'maxMessageSize
  = Data.ProtoLens.Field.field @"maybe'maxMessageSize"
maybe'message ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'message" a) =>
  Lens.Family2.LensLike' f s a
maybe'message = Data.ProtoLens.Field.field @"maybe'message"
maybe'messageId ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'messageId" a) =>
  Lens.Family2.LensLike' f s a
maybe'messageId = Data.ProtoLens.Field.field @"maybe'messageId"
maybe'messagePublishTime ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'messagePublishTime" a) =>
  Lens.Family2.LensLike' f s a
maybe'messagePublishTime
  = Data.ProtoLens.Field.field @"maybe'messagePublishTime"
maybe'mode ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'mode" a) =>
  Lens.Family2.LensLike' f s a
maybe'mode = Data.ProtoLens.Field.field @"maybe'mode"
maybe'msgBacklog ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'msgBacklog" a) =>
  Lens.Family2.LensLike' f s a
maybe'msgBacklog = Data.ProtoLens.Field.field @"maybe'msgBacklog"
maybe'msgRateExpired ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'msgRateExpired" a) =>
  Lens.Family2.LensLike' f s a
maybe'msgRateExpired
  = Data.ProtoLens.Field.field @"maybe'msgRateExpired"
maybe'msgRateOut ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'msgRateOut" a) =>
  Lens.Family2.LensLike' f s a
maybe'msgRateOut = Data.ProtoLens.Field.field @"maybe'msgRateOut"
maybe'msgRateRedeliver ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'msgRateRedeliver" a) =>
  Lens.Family2.LensLike' f s a
maybe'msgRateRedeliver
  = Data.ProtoLens.Field.field @"maybe'msgRateRedeliver"
maybe'msgThroughputOut ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'msgThroughputOut" a) =>
  Lens.Family2.LensLike' f s a
maybe'msgThroughputOut
  = Data.ProtoLens.Field.field @"maybe'msgThroughputOut"
maybe'newTxn ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'newTxn" a) =>
  Lens.Family2.LensLike' f s a
maybe'newTxn = Data.ProtoLens.Field.field @"maybe'newTxn"
maybe'newTxnResponse ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'newTxnResponse" a) =>
  Lens.Family2.LensLike' f s a
maybe'newTxnResponse
  = Data.ProtoLens.Field.field @"maybe'newTxnResponse"
maybe'nullPartitionKey ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'nullPartitionKey" a) =>
  Lens.Family2.LensLike' f s a
maybe'nullPartitionKey
  = Data.ProtoLens.Field.field @"maybe'nullPartitionKey"
maybe'nullValue ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'nullValue" a) =>
  Lens.Family2.LensLike' f s a
maybe'nullValue = Data.ProtoLens.Field.field @"maybe'nullValue"
maybe'numChunksFromMsg ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'numChunksFromMsg" a) =>
  Lens.Family2.LensLike' f s a
maybe'numChunksFromMsg
  = Data.ProtoLens.Field.field @"maybe'numChunksFromMsg"
maybe'numMessages ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'numMessages" a) =>
  Lens.Family2.LensLike' f s a
maybe'numMessages = Data.ProtoLens.Field.field @"maybe'numMessages"
maybe'numMessagesInBatch ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'numMessagesInBatch" a) =>
  Lens.Family2.LensLike' f s a
maybe'numMessagesInBatch
  = Data.ProtoLens.Field.field @"maybe'numMessagesInBatch"
maybe'orderingKey ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'orderingKey" a) =>
  Lens.Family2.LensLike' f s a
maybe'orderingKey = Data.ProtoLens.Field.field @"maybe'orderingKey"
maybe'originalAuthData ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'originalAuthData" a) =>
  Lens.Family2.LensLike' f s a
maybe'originalAuthData
  = Data.ProtoLens.Field.field @"maybe'originalAuthData"
maybe'originalAuthMethod ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'originalAuthMethod" a) =>
  Lens.Family2.LensLike' f s a
maybe'originalAuthMethod
  = Data.ProtoLens.Field.field @"maybe'originalAuthMethod"
maybe'originalPrincipal ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'originalPrincipal" a) =>
  Lens.Family2.LensLike' f s a
maybe'originalPrincipal
  = Data.ProtoLens.Field.field @"maybe'originalPrincipal"
maybe'partition ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'partition" a) =>
  Lens.Family2.LensLike' f s a
maybe'partition = Data.ProtoLens.Field.field @"maybe'partition"
maybe'partitionKey ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'partitionKey" a) =>
  Lens.Family2.LensLike' f s a
maybe'partitionKey
  = Data.ProtoLens.Field.field @"maybe'partitionKey"
maybe'partitionKeyB64Encoded ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'partitionKeyB64Encoded" a) =>
  Lens.Family2.LensLike' f s a
maybe'partitionKeyB64Encoded
  = Data.ProtoLens.Field.field @"maybe'partitionKeyB64Encoded"
maybe'partitionMetadata ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'partitionMetadata" a) =>
  Lens.Family2.LensLike' f s a
maybe'partitionMetadata
  = Data.ProtoLens.Field.field @"maybe'partitionMetadata"
maybe'partitionMetadataResponse ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'partitionMetadataResponse" a) =>
  Lens.Family2.LensLike' f s a
maybe'partitionMetadataResponse
  = Data.ProtoLens.Field.field @"maybe'partitionMetadataResponse"
maybe'partitions ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'partitions" a) =>
  Lens.Family2.LensLike' f s a
maybe'partitions = Data.ProtoLens.Field.field @"maybe'partitions"
maybe'ping ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'ping" a) =>
  Lens.Family2.LensLike' f s a
maybe'ping = Data.ProtoLens.Field.field @"maybe'ping"
maybe'pong ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'pong" a) =>
  Lens.Family2.LensLike' f s a
maybe'pong = Data.ProtoLens.Field.field @"maybe'pong"
maybe'priorityLevel ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'priorityLevel" a) =>
  Lens.Family2.LensLike' f s a
maybe'priorityLevel
  = Data.ProtoLens.Field.field @"maybe'priorityLevel"
maybe'producer ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'producer" a) =>
  Lens.Family2.LensLike' f s a
maybe'producer = Data.ProtoLens.Field.field @"maybe'producer"
maybe'producerName ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'producerName" a) =>
  Lens.Family2.LensLike' f s a
maybe'producerName
  = Data.ProtoLens.Field.field @"maybe'producerName"
maybe'producerSuccess ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'producerSuccess" a) =>
  Lens.Family2.LensLike' f s a
maybe'producerSuccess
  = Data.ProtoLens.Field.field @"maybe'producerSuccess"
maybe'protocolVersion ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'protocolVersion" a) =>
  Lens.Family2.LensLike' f s a
maybe'protocolVersion
  = Data.ProtoLens.Field.field @"maybe'protocolVersion"
maybe'proxyThroughServiceUrl ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'proxyThroughServiceUrl" a) =>
  Lens.Family2.LensLike' f s a
maybe'proxyThroughServiceUrl
  = Data.ProtoLens.Field.field @"maybe'proxyThroughServiceUrl"
maybe'proxyToBrokerUrl ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'proxyToBrokerUrl" a) =>
  Lens.Family2.LensLike' f s a
maybe'proxyToBrokerUrl
  = Data.ProtoLens.Field.field @"maybe'proxyToBrokerUrl"
maybe'reachedEndOfTopic ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'reachedEndOfTopic" a) =>
  Lens.Family2.LensLike' f s a
maybe'reachedEndOfTopic
  = Data.ProtoLens.Field.field @"maybe'reachedEndOfTopic"
maybe'readCompacted ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'readCompacted" a) =>
  Lens.Family2.LensLike' f s a
maybe'readCompacted
  = Data.ProtoLens.Field.field @"maybe'readCompacted"
maybe'redeliverUnacknowledgedMessages ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'redeliverUnacknowledgedMessages" a) =>
  Lens.Family2.LensLike' f s a
maybe'redeliverUnacknowledgedMessages
  = Data.ProtoLens.Field.field
      @"maybe'redeliverUnacknowledgedMessages"
maybe'redeliveryCount ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'redeliveryCount" a) =>
  Lens.Family2.LensLike' f s a
maybe'redeliveryCount
  = Data.ProtoLens.Field.field @"maybe'redeliveryCount"
maybe'replicateSubscriptionState ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'replicateSubscriptionState" a) =>
  Lens.Family2.LensLike' f s a
maybe'replicateSubscriptionState
  = Data.ProtoLens.Field.field @"maybe'replicateSubscriptionState"
maybe'replicatedFrom ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'replicatedFrom" a) =>
  Lens.Family2.LensLike' f s a
maybe'replicatedFrom
  = Data.ProtoLens.Field.field @"maybe'replicatedFrom"
maybe'response ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'response" a) =>
  Lens.Family2.LensLike' f s a
maybe'response = Data.ProtoLens.Field.field @"maybe'response"
maybe'schema ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'schema" a) =>
  Lens.Family2.LensLike' f s a
maybe'schema = Data.ProtoLens.Field.field @"maybe'schema"
maybe'schemaVersion ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'schemaVersion" a) =>
  Lens.Family2.LensLike' f s a
maybe'schemaVersion
  = Data.ProtoLens.Field.field @"maybe'schemaVersion"
maybe'seek ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'seek" a) =>
  Lens.Family2.LensLike' f s a
maybe'seek = Data.ProtoLens.Field.field @"maybe'seek"
maybe'send ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'send" a) =>
  Lens.Family2.LensLike' f s a
maybe'send = Data.ProtoLens.Field.field @"maybe'send"
maybe'sendError ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'sendError" a) =>
  Lens.Family2.LensLike' f s a
maybe'sendError = Data.ProtoLens.Field.field @"maybe'sendError"
maybe'sendReceipt ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'sendReceipt" a) =>
  Lens.Family2.LensLike' f s a
maybe'sendReceipt = Data.ProtoLens.Field.field @"maybe'sendReceipt"
maybe'sequenceId ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'sequenceId" a) =>
  Lens.Family2.LensLike' f s a
maybe'sequenceId = Data.ProtoLens.Field.field @"maybe'sequenceId"
maybe'serverVersion ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'serverVersion" a) =>
  Lens.Family2.LensLike' f s a
maybe'serverVersion
  = Data.ProtoLens.Field.field @"maybe'serverVersion"
maybe'startMessageId ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'startMessageId" a) =>
  Lens.Family2.LensLike' f s a
maybe'startMessageId
  = Data.ProtoLens.Field.field @"maybe'startMessageId"
maybe'startMessageRollbackDurationSec ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'startMessageRollbackDurationSec" a) =>
  Lens.Family2.LensLike' f s a
maybe'startMessageRollbackDurationSec
  = Data.ProtoLens.Field.field
      @"maybe'startMessageRollbackDurationSec"
maybe'subscribe ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'subscribe" a) =>
  Lens.Family2.LensLike' f s a
maybe'subscribe = Data.ProtoLens.Field.field @"maybe'subscribe"
maybe'subscription ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'subscription" a) =>
  Lens.Family2.LensLike' f s a
maybe'subscription
  = Data.ProtoLens.Field.field @"maybe'subscription"
maybe'success ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'success" a) =>
  Lens.Family2.LensLike' f s a
maybe'success = Data.ProtoLens.Field.field @"maybe'success"
maybe'supportsAuthRefresh ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'supportsAuthRefresh" a) =>
  Lens.Family2.LensLike' f s a
maybe'supportsAuthRefresh
  = Data.ProtoLens.Field.field @"maybe'supportsAuthRefresh"
maybe'tcId ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'tcId" a) =>
  Lens.Family2.LensLike' f s a
maybe'tcId = Data.ProtoLens.Field.field @"maybe'tcId"
maybe'topic ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'topic" a) =>
  Lens.Family2.LensLike' f s a
maybe'topic = Data.ProtoLens.Field.field @"maybe'topic"
maybe'totalChunkMsgSize ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'totalChunkMsgSize" a) =>
  Lens.Family2.LensLike' f s a
maybe'totalChunkMsgSize
  = Data.ProtoLens.Field.field @"maybe'totalChunkMsgSize"
maybe'txnAction ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'txnAction" a) =>
  Lens.Family2.LensLike' f s a
maybe'txnAction = Data.ProtoLens.Field.field @"maybe'txnAction"
maybe'txnTtlSeconds ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'txnTtlSeconds" a) =>
  Lens.Family2.LensLike' f s a
maybe'txnTtlSeconds
  = Data.ProtoLens.Field.field @"maybe'txnTtlSeconds"
maybe'txnidLeastBits ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'txnidLeastBits" a) =>
  Lens.Family2.LensLike' f s a
maybe'txnidLeastBits
  = Data.ProtoLens.Field.field @"maybe'txnidLeastBits"
maybe'txnidMostBits ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'txnidMostBits" a) =>
  Lens.Family2.LensLike' f s a
maybe'txnidMostBits
  = Data.ProtoLens.Field.field @"maybe'txnidMostBits"
maybe'type' ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'type'" a) =>
  Lens.Family2.LensLike' f s a
maybe'type' = Data.ProtoLens.Field.field @"maybe'type'"
maybe'unackedMessages ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'unackedMessages" a) =>
  Lens.Family2.LensLike' f s a
maybe'unackedMessages
  = Data.ProtoLens.Field.field @"maybe'unackedMessages"
maybe'uncompressedSize ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'uncompressedSize" a) =>
  Lens.Family2.LensLike' f s a
maybe'uncompressedSize
  = Data.ProtoLens.Field.field @"maybe'uncompressedSize"
maybe'unsubscribe ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'unsubscribe" a) =>
  Lens.Family2.LensLike' f s a
maybe'unsubscribe = Data.ProtoLens.Field.field @"maybe'unsubscribe"
maybe'userProvidedProducerName ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'userProvidedProducerName" a) =>
  Lens.Family2.LensLike' f s a
maybe'userProvidedProducerName
  = Data.ProtoLens.Field.field @"maybe'userProvidedProducerName"
maybe'uuid ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'uuid" a) =>
  Lens.Family2.LensLike' f s a
maybe'uuid = Data.ProtoLens.Field.field @"maybe'uuid"
maybe'validationError ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "maybe'validationError" a) =>
  Lens.Family2.LensLike' f s a
maybe'validationError
  = Data.ProtoLens.Field.field @"maybe'validationError"
message ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "message" a) =>
  Lens.Family2.LensLike' f s a
message = Data.ProtoLens.Field.field @"message"
messageId ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "messageId" a) =>
  Lens.Family2.LensLike' f s a
messageId = Data.ProtoLens.Field.field @"messageId"
messageIds ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "messageIds" a) =>
  Lens.Family2.LensLike' f s a
messageIds = Data.ProtoLens.Field.field @"messageIds"
messagePermits ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "messagePermits" a) =>
  Lens.Family2.LensLike' f s a
messagePermits = Data.ProtoLens.Field.field @"messagePermits"
messagePublishTime ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "messagePublishTime" a) =>
  Lens.Family2.LensLike' f s a
messagePublishTime
  = Data.ProtoLens.Field.field @"messagePublishTime"
metadata ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "metadata" a) =>
  Lens.Family2.LensLike' f s a
metadata = Data.ProtoLens.Field.field @"metadata"
mode ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "mode" a) =>
  Lens.Family2.LensLike' f s a
mode = Data.ProtoLens.Field.field @"mode"
msgBacklog ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "msgBacklog" a) =>
  Lens.Family2.LensLike' f s a
msgBacklog = Data.ProtoLens.Field.field @"msgBacklog"
msgRateExpired ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "msgRateExpired" a) =>
  Lens.Family2.LensLike' f s a
msgRateExpired = Data.ProtoLens.Field.field @"msgRateExpired"
msgRateOut ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "msgRateOut" a) =>
  Lens.Family2.LensLike' f s a
msgRateOut = Data.ProtoLens.Field.field @"msgRateOut"
msgRateRedeliver ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "msgRateRedeliver" a) =>
  Lens.Family2.LensLike' f s a
msgRateRedeliver = Data.ProtoLens.Field.field @"msgRateRedeliver"
msgThroughputOut ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "msgThroughputOut" a) =>
  Lens.Family2.LensLike' f s a
msgThroughputOut = Data.ProtoLens.Field.field @"msgThroughputOut"
name ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "name" a) =>
  Lens.Family2.LensLike' f s a
name = Data.ProtoLens.Field.field @"name"
namespace ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "namespace" a) =>
  Lens.Family2.LensLike' f s a
namespace = Data.ProtoLens.Field.field @"namespace"
newTxn ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "newTxn" a) =>
  Lens.Family2.LensLike' f s a
newTxn = Data.ProtoLens.Field.field @"newTxn"
newTxnResponse ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "newTxnResponse" a) =>
  Lens.Family2.LensLike' f s a
newTxnResponse = Data.ProtoLens.Field.field @"newTxnResponse"
nullPartitionKey ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "nullPartitionKey" a) =>
  Lens.Family2.LensLike' f s a
nullPartitionKey = Data.ProtoLens.Field.field @"nullPartitionKey"
nullValue ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "nullValue" a) =>
  Lens.Family2.LensLike' f s a
nullValue = Data.ProtoLens.Field.field @"nullValue"
numChunksFromMsg ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "numChunksFromMsg" a) =>
  Lens.Family2.LensLike' f s a
numChunksFromMsg = Data.ProtoLens.Field.field @"numChunksFromMsg"
numMessages ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "numMessages" a) =>
  Lens.Family2.LensLike' f s a
numMessages = Data.ProtoLens.Field.field @"numMessages"
numMessagesInBatch ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "numMessagesInBatch" a) =>
  Lens.Family2.LensLike' f s a
numMessagesInBatch
  = Data.ProtoLens.Field.field @"numMessagesInBatch"
orderingKey ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "orderingKey" a) =>
  Lens.Family2.LensLike' f s a
orderingKey = Data.ProtoLens.Field.field @"orderingKey"
originalAuthData ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "originalAuthData" a) =>
  Lens.Family2.LensLike' f s a
originalAuthData = Data.ProtoLens.Field.field @"originalAuthData"
originalAuthMethod ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "originalAuthMethod" a) =>
  Lens.Family2.LensLike' f s a
originalAuthMethod
  = Data.ProtoLens.Field.field @"originalAuthMethod"
originalPrincipal ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "originalPrincipal" a) =>
  Lens.Family2.LensLike' f s a
originalPrincipal = Data.ProtoLens.Field.field @"originalPrincipal"
partition ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "partition" a) =>
  Lens.Family2.LensLike' f s a
partition = Data.ProtoLens.Field.field @"partition"
partitionKey ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "partitionKey" a) =>
  Lens.Family2.LensLike' f s a
partitionKey = Data.ProtoLens.Field.field @"partitionKey"
partitionKeyB64Encoded ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "partitionKeyB64Encoded" a) =>
  Lens.Family2.LensLike' f s a
partitionKeyB64Encoded
  = Data.ProtoLens.Field.field @"partitionKeyB64Encoded"
partitionMetadata ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "partitionMetadata" a) =>
  Lens.Family2.LensLike' f s a
partitionMetadata = Data.ProtoLens.Field.field @"partitionMetadata"
partitionMetadataResponse ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "partitionMetadataResponse" a) =>
  Lens.Family2.LensLike' f s a
partitionMetadataResponse
  = Data.ProtoLens.Field.field @"partitionMetadataResponse"
partitions ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "partitions" a) =>
  Lens.Family2.LensLike' f s a
partitions = Data.ProtoLens.Field.field @"partitions"
payloadSize ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "payloadSize" a) =>
  Lens.Family2.LensLike' f s a
payloadSize = Data.ProtoLens.Field.field @"payloadSize"
ping ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "ping" a) =>
  Lens.Family2.LensLike' f s a
ping = Data.ProtoLens.Field.field @"ping"
pong ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "pong" a) =>
  Lens.Family2.LensLike' f s a
pong = Data.ProtoLens.Field.field @"pong"
priorityLevel ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "priorityLevel" a) =>
  Lens.Family2.LensLike' f s a
priorityLevel = Data.ProtoLens.Field.field @"priorityLevel"
producer ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "producer" a) =>
  Lens.Family2.LensLike' f s a
producer = Data.ProtoLens.Field.field @"producer"
producerId ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "producerId" a) =>
  Lens.Family2.LensLike' f s a
producerId = Data.ProtoLens.Field.field @"producerId"
producerName ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "producerName" a) =>
  Lens.Family2.LensLike' f s a
producerName = Data.ProtoLens.Field.field @"producerName"
producerSuccess ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "producerSuccess" a) =>
  Lens.Family2.LensLike' f s a
producerSuccess = Data.ProtoLens.Field.field @"producerSuccess"
properties ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "properties" a) =>
  Lens.Family2.LensLike' f s a
properties = Data.ProtoLens.Field.field @"properties"
protocolVersion ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "protocolVersion" a) =>
  Lens.Family2.LensLike' f s a
protocolVersion = Data.ProtoLens.Field.field @"protocolVersion"
proxyThroughServiceUrl ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "proxyThroughServiceUrl" a) =>
  Lens.Family2.LensLike' f s a
proxyThroughServiceUrl
  = Data.ProtoLens.Field.field @"proxyThroughServiceUrl"
proxyToBrokerUrl ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "proxyToBrokerUrl" a) =>
  Lens.Family2.LensLike' f s a
proxyToBrokerUrl = Data.ProtoLens.Field.field @"proxyToBrokerUrl"
publishTime ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "publishTime" a) =>
  Lens.Family2.LensLike' f s a
publishTime = Data.ProtoLens.Field.field @"publishTime"
reachedEndOfTopic ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "reachedEndOfTopic" a) =>
  Lens.Family2.LensLike' f s a
reachedEndOfTopic = Data.ProtoLens.Field.field @"reachedEndOfTopic"
readCompacted ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "readCompacted" a) =>
  Lens.Family2.LensLike' f s a
readCompacted = Data.ProtoLens.Field.field @"readCompacted"
redeliverUnacknowledgedMessages ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "redeliverUnacknowledgedMessages" a) =>
  Lens.Family2.LensLike' f s a
redeliverUnacknowledgedMessages
  = Data.ProtoLens.Field.field @"redeliverUnacknowledgedMessages"
redeliveryCount ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "redeliveryCount" a) =>
  Lens.Family2.LensLike' f s a
redeliveryCount = Data.ProtoLens.Field.field @"redeliveryCount"
replicateSubscriptionState ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "replicateSubscriptionState" a) =>
  Lens.Family2.LensLike' f s a
replicateSubscriptionState
  = Data.ProtoLens.Field.field @"replicateSubscriptionState"
replicateTo ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "replicateTo" a) =>
  Lens.Family2.LensLike' f s a
replicateTo = Data.ProtoLens.Field.field @"replicateTo"
replicatedFrom ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "replicatedFrom" a) =>
  Lens.Family2.LensLike' f s a
replicatedFrom = Data.ProtoLens.Field.field @"replicatedFrom"
requestId ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "requestId" a) =>
  Lens.Family2.LensLike' f s a
requestId = Data.ProtoLens.Field.field @"requestId"
response ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "response" a) =>
  Lens.Family2.LensLike' f s a
response = Data.ProtoLens.Field.field @"response"
schema ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "schema" a) =>
  Lens.Family2.LensLike' f s a
schema = Data.ProtoLens.Field.field @"schema"
schemaData ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "schemaData" a) =>
  Lens.Family2.LensLike' f s a
schemaData = Data.ProtoLens.Field.field @"schemaData"
schemaVersion ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "schemaVersion" a) =>
  Lens.Family2.LensLike' f s a
schemaVersion = Data.ProtoLens.Field.field @"schemaVersion"
seek ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "seek" a) =>
  Lens.Family2.LensLike' f s a
seek = Data.ProtoLens.Field.field @"seek"
send ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "send" a) =>
  Lens.Family2.LensLike' f s a
send = Data.ProtoLens.Field.field @"send"
sendError ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "sendError" a) =>
  Lens.Family2.LensLike' f s a
sendError = Data.ProtoLens.Field.field @"sendError"
sendReceipt ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "sendReceipt" a) =>
  Lens.Family2.LensLike' f s a
sendReceipt = Data.ProtoLens.Field.field @"sendReceipt"
sequenceId ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "sequenceId" a) =>
  Lens.Family2.LensLike' f s a
sequenceId = Data.ProtoLens.Field.field @"sequenceId"
serverVersion ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "serverVersion" a) =>
  Lens.Family2.LensLike' f s a
serverVersion = Data.ProtoLens.Field.field @"serverVersion"
start ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "start" a) =>
  Lens.Family2.LensLike' f s a
start = Data.ProtoLens.Field.field @"start"
startMessageId ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "startMessageId" a) =>
  Lens.Family2.LensLike' f s a
startMessageId = Data.ProtoLens.Field.field @"startMessageId"
startMessageRollbackDurationSec ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "startMessageRollbackDurationSec" a) =>
  Lens.Family2.LensLike' f s a
startMessageRollbackDurationSec
  = Data.ProtoLens.Field.field @"startMessageRollbackDurationSec"
subType ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "subType" a) =>
  Lens.Family2.LensLike' f s a
subType = Data.ProtoLens.Field.field @"subType"
subscribe ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "subscribe" a) =>
  Lens.Family2.LensLike' f s a
subscribe = Data.ProtoLens.Field.field @"subscribe"
subscription ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "subscription" a) =>
  Lens.Family2.LensLike' f s a
subscription = Data.ProtoLens.Field.field @"subscription"
success ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "success" a) =>
  Lens.Family2.LensLike' f s a
success = Data.ProtoLens.Field.field @"success"
supportsAuthRefresh ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "supportsAuthRefresh" a) =>
  Lens.Family2.LensLike' f s a
supportsAuthRefresh
  = Data.ProtoLens.Field.field @"supportsAuthRefresh"
tcId ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "tcId" a) =>
  Lens.Family2.LensLike' f s a
tcId = Data.ProtoLens.Field.field @"tcId"
topic ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "topic" a) =>
  Lens.Family2.LensLike' f s a
topic = Data.ProtoLens.Field.field @"topic"
topics ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "topics" a) =>
  Lens.Family2.LensLike' f s a
topics = Data.ProtoLens.Field.field @"topics"
totalChunkMsgSize ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "totalChunkMsgSize" a) =>
  Lens.Family2.LensLike' f s a
totalChunkMsgSize = Data.ProtoLens.Field.field @"totalChunkMsgSize"
txnAction ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "txnAction" a) =>
  Lens.Family2.LensLike' f s a
txnAction = Data.ProtoLens.Field.field @"txnAction"
txnTtlSeconds ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "txnTtlSeconds" a) =>
  Lens.Family2.LensLike' f s a
txnTtlSeconds = Data.ProtoLens.Field.field @"txnTtlSeconds"
txnidLeastBits ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "txnidLeastBits" a) =>
  Lens.Family2.LensLike' f s a
txnidLeastBits = Data.ProtoLens.Field.field @"txnidLeastBits"
txnidMostBits ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "txnidMostBits" a) =>
  Lens.Family2.LensLike' f s a
txnidMostBits = Data.ProtoLens.Field.field @"txnidMostBits"
type' ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "type'" a) =>
  Lens.Family2.LensLike' f s a
type' = Data.ProtoLens.Field.field @"type'"
unackedMessages ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "unackedMessages" a) =>
  Lens.Family2.LensLike' f s a
unackedMessages = Data.ProtoLens.Field.field @"unackedMessages"
uncompressedSize ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "uncompressedSize" a) =>
  Lens.Family2.LensLike' f s a
uncompressedSize = Data.ProtoLens.Field.field @"uncompressedSize"
unsubscribe ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "unsubscribe" a) =>
  Lens.Family2.LensLike' f s a
unsubscribe = Data.ProtoLens.Field.field @"unsubscribe"
userProvidedProducerName ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "userProvidedProducerName" a) =>
  Lens.Family2.LensLike' f s a
userProvidedProducerName
  = Data.ProtoLens.Field.field @"userProvidedProducerName"
uuid ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "uuid" a) =>
  Lens.Family2.LensLike' f s a
uuid = Data.ProtoLens.Field.field @"uuid"
validationError ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "validationError" a) =>
  Lens.Family2.LensLike' f s a
validationError = Data.ProtoLens.Field.field @"validationError"
value ::
  forall f s a.
  (Prelude.Functor f, Data.ProtoLens.Field.HasField s "value" a) =>
  Lens.Family2.LensLike' f s a
value = Data.ProtoLens.Field.field @"value"
vec'ackSet ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'ackSet" a) =>
  Lens.Family2.LensLike' f s a
vec'ackSet = Data.ProtoLens.Field.field @"vec'ackSet"
vec'encryptionKeys ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'encryptionKeys" a) =>
  Lens.Family2.LensLike' f s a
vec'encryptionKeys
  = Data.ProtoLens.Field.field @"vec'encryptionKeys"
vec'hashRanges ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'hashRanges" a) =>
  Lens.Family2.LensLike' f s a
vec'hashRanges = Data.ProtoLens.Field.field @"vec'hashRanges"
vec'messageId ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'messageId" a) =>
  Lens.Family2.LensLike' f s a
vec'messageId = Data.ProtoLens.Field.field @"vec'messageId"
vec'messageIds ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'messageIds" a) =>
  Lens.Family2.LensLike' f s a
vec'messageIds = Data.ProtoLens.Field.field @"vec'messageIds"
vec'metadata ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'metadata" a) =>
  Lens.Family2.LensLike' f s a
vec'metadata = Data.ProtoLens.Field.field @"vec'metadata"
vec'partitions ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'partitions" a) =>
  Lens.Family2.LensLike' f s a
vec'partitions = Data.ProtoLens.Field.field @"vec'partitions"
vec'properties ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'properties" a) =>
  Lens.Family2.LensLike' f s a
vec'properties = Data.ProtoLens.Field.field @"vec'properties"
vec'replicateTo ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'replicateTo" a) =>
  Lens.Family2.LensLike' f s a
vec'replicateTo = Data.ProtoLens.Field.field @"vec'replicateTo"
vec'subscription ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'subscription" a) =>
  Lens.Family2.LensLike' f s a
vec'subscription = Data.ProtoLens.Field.field @"vec'subscription"
vec'topics ::
  forall f s a.
  (Prelude.Functor f,
   Data.ProtoLens.Field.HasField s "vec'topics" a) =>
  Lens.Family2.LensLike' f s a
vec'topics = Data.ProtoLens.Field.field @"vec'topics"