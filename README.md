hpulsar
=======

[Apache Pulsar](https://pulsar.apache.org/) client for Haskell.

### Build

Within a Nix shell (run `nix-shell` at the project's root).

```shell
cabal new-build
```

### Run the example

The example in `app/Main.hs` simply connects to a local Pulsar instance, sends a PING command followed by creating a PRODUCER.

```shell
cabal new-run
```

You should see an output similar to the one below in Pulsar.

```
pulsar_1          | 15:10:50.125 [pulsar-io-50-1] INFO  org.apache.pulsar.broker.service.ServerCnx - New connection from /172.25.0.1:35310
pulsar_1          | 15:10:50.126 [pulsar-io-50-1] INFO  org.apache.pulsar.broker.service.ServerCnx - [/172.25.0.1:35310][non-persistent://public/default/test] Creating producer. producerId=0
pulsar_1          | 15:10:50.126 [pulsar-io-50-1] INFO  org.apache.pulsar.broker.service.ServerCnx - Closed connection from /172.25.0.1:35310
```
