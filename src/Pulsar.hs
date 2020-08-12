{-# LANGUAGE OverloadedStrings #-}

module Pulsar where

import           Data.ProtoLens                 ( defMessage )
import qualified Data.Text                     as T
import           Data.Version                   ( showVersion )
import           Lens.Family
import           Paths_hpulsar                  ( version )
import           Proto.PulsarApi
import qualified Proto.PulsarApi_Fields        as F
import           TCPClient                      ( send )

cmdConnect :: BaseCommand
cmdConnect = defMessage & F.connect .~ connect
 where
  connect :: CommandConnect
  connect = defMessage
    & F.clientVersion .~ "Pulsar-Client-Haskell-v" <> T.pack (showVersion version)
    & F.protocolVersion .~ 15

cmdSubscribe :: BaseCommand
cmdSubscribe =
  defMessage
    & F.type' .~ BaseCommand'SUBSCRIBE
    & F.subscribe .~ subscribe
 where
  subscribe :: CommandSubscribe
  subscribe = defMessage
    & F.topic .~ "foo"
    & F.subscription .~ "foo-subscription"
    & F.subType .~ CommandSubscribe'Shared

cmdProducer :: BaseCommand
cmdProducer =
  defMessage
    & F.type' .~ BaseCommand'PRODUCER
    & F.producer .~ producer
 where
  producer :: CommandProducer
  producer = defMessage & F.topic .~ "foo"

test :: IO ()
test = do
  print cmdConnect
  print cmdProducer
  print cmdSubscribe
  send cmdConnect
