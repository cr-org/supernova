{-# LANGUAGE OverloadedStrings #-}

module Pulsar where

import           Data.ProtoLens                 ( defMessage )
import qualified Data.Text                     as T
import           Data.Version                   ( showVersion )
import           Lens.Family
import           Paths_hpulsar                  ( version )
import           Proto.PulsarApi
import qualified Proto.PulsarApi_Fields        as F

cmdConnect :: CommandConnect
cmdConnect =
  defMessage
    & F.clientVersion .~ "Pulsar-Client-Haskell-v" <> T.pack (showVersion version)
    & F.protocolVersion .~ 15

cmdSubType :: CommandSubscribe'SubType
cmdSubType = CommandSubscribe'Shared

cmdSubscribe :: CommandSubscribe
cmdSubscribe =
  defMessage
    & F.topic .~ "foo"
    & F.subscription .~ "foo-subscription"
    & F.subType .~ cmdSubType

cmdProducer :: CommandProducer
cmdProducer =
  defMessage
    & F.topic .~ "foo"

test :: IO ()
test = do
  print cmdConnect
  print cmdProducer
  print cmdSubscribe
