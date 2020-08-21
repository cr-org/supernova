{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Main where

import           Control.Concurrent             ( threadDelay )
import           Control.Concurrent.Async       ( concurrently_ )
import           Control.Monad                  ( forever )
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8    as CL
import           Data.Foldable                  ( traverse_ )
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )
import           Pulsar
import           Streamly                       ( asyncly
                                                , maxThreads
                                                )
import qualified Streamly.Prelude              as S

main :: IO ()
main = demo

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
  in  putStrLn $ "-----------------> " <> show msg

topic :: Topic
topic = defaultTopic "app"

demo :: IO ()
demo = runPulsar resources $ \(Consumer {..}, Producer {..}) ->
  let c = forever $ fetch >>= \(Message i m) -> msgDecoder m >> ack i
      p = forever $ sleep 5 >> traverse_ produce messages
  in  concurrently_ c p

resources :: Pulsar (Consumer IO Message, Producer IO)
resources = do
  ctx      <- connect defaultConnectData
  consumer <- newConsumer ctx topic "test-sub"
  producer <- newProducer ctx topic
  return (consumer, producer)

sleep :: Int -> IO ()
sleep n = threadDelay (n * 1000000)

logOpts :: LogOptions
logOpts = LogOptions Info StdOut

streamDemo :: IO ()
streamDemo = runPulsar' logOpts resources $ \(Consumer {..}, Producer {..}) ->
  let c = forever $ fetch >>= \(Message i m) -> msgDecoder m >> ack i
      p = forever $ sleep 5 >> traverse_ produce messages
  in  S.drain . asyncly . maxThreads 10 $ S.yieldM c <> S.yieldM p
