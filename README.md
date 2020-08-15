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

### Run the example

The example in `app/Main.hs` simply connects to a local Pulsar instance and emits some simple commands.

```shell
cabal new-run
```

You should see an output similar to the one below.

```
[ Establishing connection with Pulsar ]
<<< {server_version: "Pulsar Server" protocol_version: 15 max_message_size: 5242880}
>>> {type: PING ping { }}
<<< {type: PONG pong { }}
>>> {type: LOOKUP lookupTopic { topic: "non-persistent://public/default/app" request_id: 0 }}
<<< {type: LOOKUP_RESPONSE lookupTopicResponse { brokerServiceUrl: "pulsar://localhost:6650" response: Connect request_id: 0 authoritative: true proxy_through_service_url: true }}
>>> {type: PRODUCER producer { topic: "non-persistent://public/default/app" producer_id: 0 request_id: 0 }}
<<< {type: PRODUCER_SUCCESS producer_success { request_id: 0 producer_name: "standalone-1-26" last_sequence_id: -1 schema_version: "" }}
>>> {type: SEND send { producer_id: 0 sequence_id: 0 num_messages: 1 }}
<<< {type: SEND_RECEIPT send_receipt { producer_id: 0 sequence_id: 0 message_id { ledgerId: 0 entryId: 0 } highest_sequence_id: 0 }}
>>> {type: CLOSE_PRODUCER close_producer { producer_id: 0 request_id: 0 }}
<<< {type: SUCCESS success { request_id: 0 }}
[ Closing Pulsar connection ]
```

And something like this in the Pulsar logs.

```
pulsar_1          | 09:56:20.387 [pulsar-io-50-1] INFO  org.apache.pulsar.broker.service.ServerCnx - New connection from /172.26.0.1:53836
pulsar_1          | 09:56:22.391 [pulsar-io-50-1] INFO  org.apache.pulsar.broker.service.ServerCnx - [/172.26.0.1:53836][non-persistent://public/default/app] Creating producer. producerId=0
pulsar_1          | 09:56:22.392 [ForkJoinPool.commonPool-worker-6] INFO  org.apache.pulsar.broker.service.ServerCnx - [/172.26.0.1:53836] non-persistent://public/default/app configured with schema false
pulsar_1          | 09:56:22.393 [ForkJoinPool.commonPool-worker-6] INFO  org.apache.pulsar.broker.service.ServerCnx - [/172.26.0.1:53836] Created new producer: Producer{topic=NonPersistentTopic{topic=non-persistent://public/default/app}, client=/172.26.0.1:53836, producerName=standalone-0-70, producerId=0}
pulsar_1          | 09:56:24.396 [pulsar-io-50-1] INFO  org.apache.pulsar.broker.service.ServerCnx - [NonPersistentTopic{topic=non-persistent://public/default/app}][standalone-0-70] Closing producer on cnx /172.26.0.1:53836
pulsar_1          | 09:56:24.396 [pulsar-io-50-1] INFO  org.apache.pulsar.broker.service.ServerCnx - [NonPersistentTopic{topic=non-persistent://public/default/app}][standalone-0-70] Closed producer on cnx /172.26.0.1:53836
pulsar_1          | 09:56:24.397 [pulsar-io-50-1] INFO  org.apache.pulsar.broker.service.ServerCnx - Closed connection from /172.26.0.1:53836
```
