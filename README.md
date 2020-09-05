supernova
=========

[![CI Status](https://github.com/cr-org/supernova/workflows/Haskell%20CI/badge.svg)](https://github.com/cr-org/supernova/actions)

⚠️  **it is still very much under development 🚧 so use it at your own risk** ⚠️

A *supernova* is a powerful and luminous stellar explosion. This transient astronomical event occurs during the last evolutionary stages of a massive star or when a white dwarf is triggered into runaway nuclear fusion. The original object, called the progenitor, either collapses to a [neutron](https://github.com/cr-org/neutron) star or black hole, or is completely destroyed. The peak optical luminosity of a supernova can be comparable to that of an entire galaxy before fading over several weeks or months.

[![supernova](https://www.jpl.nasa.gov/spaceimages/images/largesize/PIA22352_hires.jpg "Kepler Beyond Planets: Finding Exploding Stars (Type Ia Supernova from a White Dwarf Stealing Matter)")](https://www.jpl.nasa.gov/spaceimages/details.php?id=PIA22352)

### Quick Start

The example located in `test/Main.hs` showcases a consumer & producer running concurrently (needs the `async` library).

```haskell
main :: IO ()
main = runPulsar conn $ do
  c <- newConsumer topic "test-sub"
  p <- newProducer topic
  liftIO $ program c p

conn :: PulsarConnection
conn = connect defaultConnectData

program :: Consumer IO -> Producer IO -> IO ()
program (Consumer fetch ack) (Producer send) =
  let c = forever $ fetch >>= \(Message i m) -> msgDecoder m >> ack i
      p = forever $ sleep 5 >> traverse_ send messages
  in  concurrently_ c p
```

A `Message` contains a `MessageID` you need for `ack`ing and a payload defined as a lazy `ByteString`.

Run it with the following command:

```shell
cabal new-run supernova-tests
```

By default, it logs to the standard output in DEBUG level. You can change it by suppling `LogOptions` to the alternative function `runPulsar'`.

```haskell
runPulsar' :: LogOptions -> PulsarConnection -> Pulsar a -> IO ()
```

### Streaming

Since both consumers and producers operate on any `MonadIO m`, we could leverage some streaming libraries. Here's the same example using [streamly](https://hackage.haskell.org/package/streamly).

```haskell
import           Streamly
import qualified Streamly.Prelude              as S

main :: IO ()
main = runPulsar conn $ do
  c <- newConsumer topic "test-sub"
  p <- newProducer topic
  liftIO $ program c p

program :: Consumer IO -> Producer IO -> IO ()
program (Consumer fetch ack) (Producer send) =
  let c = forever $ fetch >>= \(Message i m) -> msgDecoder m >> ack i
      p = forever $ sleep 5 >> traverse_ send messages
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
