hpulsar
=======

[![CI Status](https://github.com/cr-org/hpulsar/workflows/Haskell%20CI/badge.svg)](https://github.com/cr-org/hpulsar/actions)

[Apache Pulsar](https://pulsar.apache.org/) client for Haskell.

⚠️  **it is still very much under development 🚧 so use it at your own risk** ⚠️

### Build

It is recommended to use [Cachix](https://app.cachix.org/cache/hpulsar) to reduce the compilation time.

```shell
nix-build
```

Or within a Nix shell (run `nix-shell` at the project's root).

```shell
cabal new-build
```

### Consumer / Producer example

The example located in `app/Main.hs` showcases a simple consumer / producer program. Having the following data and functions:

```haskell
data Msg = Msg
  { name :: Text
  , amount :: Int
  } deriving (Generic, FromJSON, ToJSON, Show)

messages :: [PulsarMessage]
messages =
  let msg = [Msg "foo" 2, Msg "bar" 5, Msg "taz" 1]
  in  PulsarMessage . encode <$> msg

msgDecoder :: CL.ByteString -> IO ()
msgDecoder bs =
  let msg = decode bs :: Maybe Msg
  in  putStrLn $ "-------- CONSUMED: " <> show msg

topic :: Topic
topic = defaultTopic "app"
```

We can run the consumer and producer concurrently (needs the `async` library).

```haskell
main :: IO ()
main = runPulsar resources $ \(Consumer {..}, Producer {..}) ->
  let c = forever $ fetch >>= \(Message i p) -> msgDecoder p >> ack i
      p = forever $ sleep 5 >> traverse_ produce messages
  in  concurrently_ c p

resources :: Pulsar (Consumer IO Message, Producer IO)
resources = do
  ctx      <- connect defaultConnectData
  consumer <- newConsumer ctx topic "test-sub"
  producer <- newProducer ctx topic
  return (consumer, producer)
```

A `Message` contains a `MessageID` you need for `ack`ing and a payload defined as a lazy `ByteString`.

Run it with the following command:

```shell
cabal new-run
```

You should see an output similar to the one below.

```
[ Establishing connection with Pulsar ]
<<< {server_version: "Pulsar Server" protocol_version: 15 max_message_size: 5242880}
20:48:56.172842778 [DEBUG] >>> {type: LOOKUP lookupTopic { topic: "non-persistent://public/default/app" request_id: 0 }}
20:48:56.175524126 [DEBUG] <<< {type: LOOKUP_RESPONSE lookupTopicResponse { brokerServiceUrl: "pulsar://localhost:6650" response: Connect request_id: 0 authoritative: true proxy_through_service_url: true }}
20:48:56.175604598 [DEBUG] >>> {type: SUBSCRIBE subscribe { topic: "non-persistent://public/default/app" subscription: "test-sub" subType: Shared consumer_id: 0 request_id: 0 }}
20:48:56.176373101 [DEBUG] <<< {type: SUCCESS success { request_id: 0 }}
20:48:56.176440936 [DEBUG] >>> {type: FLOW flow { consumer_id: 0 messagePermits: 100 }}
20:48:56.17652437 [DEBUG] >>> {type: LOOKUP lookupTopic { topic: "non-persistent://public/default/app" request_id: 0 }}
20:48:56.176881254 [DEBUG] <<< {type: LOOKUP_RESPONSE lookupTopicResponse { brokerServiceUrl: "pulsar://localhost:6650" response: Connect request_id: 0 authoritative: true proxy_through_service_url: true }}
20:48:56.17694067 [DEBUG] >>> {type: PRODUCER producer { topic: "non-persistent://public/default/app" producer_id: 0 request_id: 0 }}
20:48:56.177940518 [DEBUG] <<< {type: PRODUCER_SUCCESS producer_success { request_id: 0 producer_name: "standalone-2-100" last_sequence_id: -1 schema_version: "" }}
20:49:01.180977441 [DEBUG] >>> {type: SEND send { producer_id: 0 sequence_id: 0 num_messages: 1 }}
20:49:01.182626154 [DEBUG] <<< {producer_id: 0 sequence_id: 0 message_id { ledgerId: 0 entryId: 0 } highest_sequence_id: 0}
20:49:01.182835961 [DEBUG] >>> {type: SEND send { producer_id: 0 sequence_id: 1 num_messages: 1 }}
20:49:01.183355175 [DEBUG] <<< {consumer_id: 0 message_id { ledgerId: 0 entryId: 0 partition: -1 }}
-----------------> Just (Msg {name = "foo", amount = 2})
20:49:01.183606788 [DEBUG] >>> {type: ACK ack { consumer_id: 0 ack_type: Individual message_id { ledgerId: 0 entryId: 0 partition: -1 } }}
20:49:01.184276652 [DEBUG] <<< {producer_id: 0 sequence_id: 1 message_id { ledgerId: 0 entryId: 0 } highest_sequence_id: 18446744073709551615}
20:49:01.184422938 [DEBUG] >>> {type: SEND send { producer_id: 0 sequence_id: 2 num_messages: 1 }}
20:49:01.184702445 [DEBUG] <<< {consumer_id: 0 message_id { ledgerId: 0 entryId: 0 partition: -1 }}
-----------------> Just (Msg {name = "bar", amount = 5})
20:49:01.184870493 [DEBUG] >>> {type: ACK ack { consumer_id: 0 ack_type: Individual message_id { ledgerId: 0 entryId: 0 partition: -1 } }}
20:49:01.18573319 [DEBUG] <<< {producer_id: 0 sequence_id: 2 message_id { ledgerId: 0 entryId: 0 } highest_sequence_id: 18446744073709551615}
20:49:01.185927028 [DEBUG] <<< {consumer_id: 0 message_id { ledgerId: 0 entryId: 0 partition: -1 }}
-----------------> Just (Msg {name = "taz", amount = 1})
20:49:01.186102654 [DEBUG] >>> {type: ACK ack { consumer_id: 0 ack_type: Individual message_id { ledgerId: 0 entryId: 0 partition: -1 } }}
^C20:49:01.959014662 [DEBUG] >>> {type: CLOSE_PRODUCER close_producer { producer_id: 0 request_id: 0 }}
20:49:01.959283423 [DEBUG] >>> {type: CLOSE_CONSUMER close_consumer { consumer_id: 0 request_id: 0 }}
20:49:01.959902614 [DEBUG] <<< {type: SUCCESS success { request_id: 0 }}
[ Closing Pulsar connection ]
```

By default, it logs to standard out in DEBUG level. You can change it by suppling `LogOptions`.

```haskell
logOpts :: LogOptions
logOpts = LogOptions Info StdOut

runPulsar' logOpts resources
```

### Streaming

Since both consumers and producers operate on any `MonadIO m`, we could leverage some streaming libraries. Here's the same example using [streamly](https://hackage.haskell.org/package/streamly).

```haskell
import           Streamly
import qualified Streamly.Prelude              as S

main :: IO ()
main = runPulsar resources $ \(Consumer {..}, Producer {..}) ->
  let c = forever $ fetch >>= \(Message i p) -> msgDecoder p >> ack i
      p = forever $ sleep 5 >> traverse_ produce messages
  in  S.drain . asyncly . maxThreads 10 $ S.yieldM c <> S.yieldM p
```
