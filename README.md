hpulsar
=======

[![CI Status](https://github.com/cr-org/hpulsar/workflows/Haskell%20CI/badge.svg)](https://github.com/cr-org/hpulsar/actions)

[Apache Pulsar](https://pulsar.apache.org/) client for Haskell.

### Build

Within a Nix shell (run `nix-shell` at the project's root).

```shell
cabal new-build
```

### Run the example

The example in `app/Main.hs` simply connects to a local Pulsar instance and emits some simple commands.

```shell
cabal new-run
```

You should see an output similar to the one below in the Pulsar logs.

```
pulsar_1          | 09:56:20.387 [pulsar-io-50-1] INFO  org.apache.pulsar.broker.service.ServerCnx - New connection from /172.26.0.1:53836
pulsar_1          | 09:56:22.391 [pulsar-io-50-1] INFO  org.apache.pulsar.broker.service.ServerCnx - [/172.26.0.1:53836][non-persistent://public/default/app] Creating producer. producerId=0
pulsar_1          | 09:56:22.392 [ForkJoinPool.commonPool-worker-6] INFO  org.apache.pulsar.broker.service.ServerCnx - [/172.26.0.1:53836] non-persistent://public/default/app configured with schema false
pulsar_1          | 09:56:22.393 [ForkJoinPool.commonPool-worker-6] INFO  org.apache.pulsar.broker.service.ServerCnx - [/172.26.0.1:53836] Created new producer: Producer{topic=NonPersistentTopic{topic=non-persistent://public/default/app}, client=/172.26.0.1:53836, producerName=standalone-0-70, producerId=0}
pulsar_1          | 09:56:24.396 [pulsar-io-50-1] INFO  org.apache.pulsar.broker.service.ServerCnx - [NonPersistentTopic{topic=non-persistent://public/default/app}][standalone-0-70] Closing producer on cnx /172.26.0.1:53836
pulsar_1          | 09:56:24.396 [pulsar-io-50-1] INFO  org.apache.pulsar.broker.service.ServerCnx - [NonPersistentTopic{topic=non-persistent://public/default/app}][standalone-0-70] Closed producer on cnx /172.26.0.1:53836
pulsar_1          | 09:56:24.397 [pulsar-io-50-1] INFO  org.apache.pulsar.broker.service.ServerCnx - Closed connection from /172.26.0.1:53836
```
