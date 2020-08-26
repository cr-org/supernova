supernova
=========

[![CI Status](https://github.com/cr-org/supernova/workflows/Haskell%20CI/badge.svg)](https://github.com/cr-org/supernova/actions)

⚠️  **it is still very much under development 🚧 so use it at your own risk** ⚠️

A *supernova* is a powerful and luminous stellar explosion. This transient astronomical event occurs during the last evolutionary stages of a massive star or when a white dwarf is triggered into runaway nuclear fusion. The original object, called the progenitor, either collapses to a [neutron](https://github.com/cr-org/neutron) star or black hole, or is completely destroyed. The peak optical luminosity of a supernova can be comparable to that of an entire galaxy before fading over several weeks or months.

[![supernova](https://www.jpl.nasa.gov/spaceimages/images/largesize/PIA22352_hires.jpg "Kepler Beyond Planets: Finding Exploding Stars (Type Ia Supernova from a White Dwarf Stealing Matter)")](https://www.jpl.nasa.gov/spaceimages/details.php?id=PIA22352)

### Quick Start

The example located in `test/Main.hs` showcases a simple consumer and producer running concurrently (needs the `async` library).

```haskell
main :: IO ()
main = runPulsar resources $ \(Consumer {..}, Producer {..}) ->
  let c = forever $ fetch >>= \(Message i p) -> msgDecoder p >> ack i
      p = forever $ sleep 5 >> traverse_ produce messages
  in  concurrently_ c p

resources :: Pulsar (Consumer IO, Producer IO)
resources = do
  ctx      <- connect defaultConnectData
  consumer <- newConsumer ctx topic "test-sub"
  producer <- newProducer ctx topic
  return (consumer, producer)
```

A `Message` contains a `MessageID` you need for `ack`ing and a payload defined as a lazy `ByteString`.

Run it with the following command:

```shell
cabal new-run supernova-tests
```

You should see an output similar to the one below.

```
[ Establishing connection with Pulsar ]
11:08:17.241223255 [DEBUG] <<< SimpleResponse {type: CONNECTED connected { server_version: "Pulsar Server" protocol_version: 15 max_message_size: 5242880 }}
11:08:17.241393781 [DEBUG] >>> {type: LOOKUP lookupTopic { topic: "non-persistent://public/default/app" request_id: 0 }}
11:08:17.287593289 [DEBUG] <<< SimpleResponse {type: LOOKUP_RESPONSE lookupTopicResponse { brokerServiceUrl: "pulsar://localhost:6650" response: Connect request_id: 0 authoritative: true proxy_through_service_url: true }}
11:08:17.287714597 [DEBUG] >>> {type: SUBSCRIBE subscribe { topic: "non-persistent://public/default/app" subscription: "test-sub" subType: Shared consumer_id: 0 request_id: 1 }}
11:08:17.515138135 [DEBUG] <<< SimpleResponse {type: SUCCESS success { request_id: 1 }}
11:08:17.515235643 [DEBUG] >>> {type: FLOW flow { consumer_id: 0 messagePermits: 100 }}
11:08:17.515366235 [DEBUG] >>> {type: LOOKUP lookupTopic { topic: "non-persistent://public/default/app" request_id: 2 }}
11:08:17.516733669 [DEBUG] <<< SimpleResponse {type: LOOKUP_RESPONSE lookupTopicResponse { brokerServiceUrl: "pulsar://localhost:6650" response: Connect request_id: 2 authoritative: true proxy_through_service_url: true }}
11:08:17.516814272 [DEBUG] >>> {type: PRODUCER producer { topic: "non-persistent://public/default/app" producer_id: 0 request_id: 3 }}
11:08:17.519747974 [DEBUG] <<< SimpleResponse {type: PRODUCER_SUCCESS producer_success { request_id: 3 producer_name: "standalone-0-3" last_sequence_id: -1 schema_version: "" }}
11:08:22.522531505 [DEBUG] >>> {type: SEND send { producer_id: 0 sequence_id: 0 }}
11:08:22.524944396 [DEBUG] <<< SimpleResponse {type: SEND_RECEIPT send_receipt { producer_id: 0 sequence_id: 0 message_id { ledgerId: 0 entryId: 0 } highest_sequence_id: 0 }}
11:08:22.525110484 [DEBUG] >>> {type: SEND send { producer_id: 0 sequence_id: 1 }}
11:08:22.525417829 [DEBUG] <<< {type: MESSAGE message { consumer_id: 0 message_id { ledgerId: 0 entryId: 0 partition: -1 } }}
-----------------> Just (Msg {name = "foo", amount = 2})
11:08:22.525728069 [DEBUG] >>> {type: ACK ack { consumer_id: 0 ack_type: Individual message_id { ledgerId: 0 entryId: 0 partition: -1 } }}
11:08:22.526928875 [DEBUG] <<< SimpleResponse {type: SEND_RECEIPT send_receipt { producer_id: 0 sequence_id: 1 message_id { ledgerId: 0 entryId: 0 } highest_sequence_id: 18446744073709551615 }}
11:08:22.527094992 [DEBUG] >>> {type: SEND send { producer_id: 0 sequence_id: 2 }}
11:08:22.527478495 [DEBUG] <<< {type: MESSAGE message { consumer_id: 0 message_id { ledgerId: 0 entryId: 0 partition: -1 } }}
-----------------> Just (Msg {name = "bar", amount = 5})
11:08:22.527755758 [DEBUG] >>> {type: ACK ack { consumer_id: 0 ack_type: Individual message_id { ledgerId: 0 entryId: 0 partition: -1 } }}
11:08:22.530046522 [DEBUG] <<< SimpleResponse {type: SEND_RECEIPT send_receipt { producer_id: 0 sequence_id: 2 message_id { ledgerId: 0 entryId: 0 } highest_sequence_id: 18446744073709551615 }}
11:08:22.530462353 [DEBUG] <<< {type: MESSAGE message { consumer_id: 0 message_id { ledgerId: 0 entryId: 0 partition: -1 } }}
-----------------> Just (Msg {name = "taz", amount = 1})
11:08:22.530749224 [DEBUG] >>> {type: ACK ack { consumer_id: 0 ack_type: Individual message_id { ledgerId: 0 entryId: 0 partition: -1 } }}
^C11:08:23.626651694 [DEBUG] >>> {type: CLOSE_PRODUCER close_producer { producer_id: 0 request_id: 4 }}
11:08:23.627778045 [DEBUG] <<< SimpleResponse {type: SUCCESS success { request_id: 4 }}
11:08:23.627851503 [DEBUG] >>> {type: CLOSE_CONSUMER close_consumer { consumer_id: 0 request_id: 5 }}
11:08:23.627902614 [DEBUG] <<< {type: SUCCESS success { request_id: 0 }}
[ Closing Pulsar connection ]
```

By default, it logs to the standard output in DEBUG level. You can change it by suppling `LogOptions`.

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

### Development

It is recommended to use [Cachix](https://app.cachix.org/cache/hpulsar) to reduce the compilation time.

```shell
nix-build
```

Or within a Nix shell (run `nix-shell` at the project's root).

```shell
cabal new-build
```
