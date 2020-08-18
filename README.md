hpulsar
=======

[![CI Status](https://github.com/cr-org/hpulsar/workflows/Haskell%20CI/badge.svg)](https://github.com/cr-org/hpulsar/actions)

[Apache Pulsar](https://pulsar.apache.org/) client for Haskell.

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

The example located in `app/Main.hs` showcases a simple consumer / producer program that runs concurrently (needs the `async` library).

```haskell
main :: IO ()
main = runPulsar resources $ \(Consumer {..}, Producer {..}) ->
  let c = forever $ fetch >>= \m -> print m >> ack m
      p = traverse_ produce ["foo", "bar", "taz"]
  in  concurrently_ c p

topic :: Topic
topic = defaultTopic "app"

resources :: Pulsar (Consumer IO, Producer IO)
resources = do
  ctx      <- connect defaultConnectData
  consumer <- newConsumer ctx topic "test-sub"
  producer <- newProducer ctx topic
  return (consumer, producer)
```

Run it with the following command:

```shell
cabal new-run
```

You should see an output similar to the one below.

```
[ Establishing connection with Pulsar ]
<<< {server_version: "Pulsar Server" protocol_version: 15 max_message_size: 5242880}
>>> {type: LOOKUP lookupTopic { topic: "non-persistent://public/default/app" request_id: 0 }}
<<< SimpleResponse {type: LOOKUP_RESPONSE lookupTopicResponse { brokerServiceUrl: "pulsar://localhost:6650" response: Connect request_id: 0 authoritative: true proxy_through_service_url: true }}
>>> {type: SUBSCRIBE subscribe { topic: "non-persistent://public/default/app" subscription: "test-sub" subType: Shared consumer_id: 0 request_id: 0 }}
<<< SimpleResponse {type: SUCCESS success { request_id: 0 }}
>>> {type: FLOW flow { consumer_id: 0 messagePermits: 100 }}
>>> {type: LOOKUP lookupTopic { topic: "non-persistent://public/default/app" request_id: 0 }}
<<< SimpleResponse {type: LOOKUP_RESPONSE lookupTopicResponse { brokerServiceUrl: "pulsar://localhost:6650" response: Connect request_id: 0 authoritative: true proxy_through_service_url: true }}
>>> {type: PRODUCER producer { topic: "non-persistent://public/default/app" producer_id: 0 request_id: 0 }}
<<< SimpleResponse {type: PRODUCER_SUCCESS producer_success { request_id: 0 producer_name: "standalone-0-92" last_sequence_id: -1 schema_version: "" }}
>>> {type: SEND send { producer_id: 0 sequence_id: 0 num_messages: 1 }}
<<< SimpleResponse {type: SEND_RECEIPT send_receipt { producer_id: 0 sequence_id: 0 message_id { ledgerId: 0 entryId: 0 } highest_sequence_id: 18446744073709551615 }}
>>> {type: SEND send { producer_id: 0 sequence_id: 0 num_messages: 1 }}
<<< PayloadResponse {type: MESSAGE message { consumer_id: 0 message_id { ledgerId: 0 entryId: 0 partition: -1 } }} {producer_name: "" sequence_id: 0 publish_time: 0} (Just (Payload "foo"))
>>> {type: CLOSE_PRODUCER close_producer { producer_id: 0 request_id: 0 }}
<<< SimpleResponse {type: SUCCESS success { request_id: 0 }}
>>> {type: CLOSE_CONSUMER close_consumer { consumer_id: 0 request_id: 0 }}
<<< SimpleResponse {type: SUCCESS success { request_id: 0 }}
[ Closing Pulsar connection ]
```
