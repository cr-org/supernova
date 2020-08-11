{-# LANGUAGE OverloadedStrings #-}

module Pulsar where

import           Data.ProtoLens                 ( defMessage )
import           Lens.Family
import           Proto.PulsarApi
import           Proto.PulsarApi_Fields         ( subscription
                                                , subType
                                                , topic
                                                )

cmdConnect :: CommandConnect
cmdConnect = defMessage

cmdSubType :: CommandSubscribe'SubType
cmdSubType = CommandSubscribe'Shared

cmdSubscribe :: CommandSubscribe
cmdSubscribe =
  defMessage
    & topic .~ "foo"
    & subscription .~ "foo-subscription"
    & subType .~ cmdSubType

cmdProducer :: CommandProducer
cmdProducer =
  defMessage
    & topic .~ "foo"

test :: IO ()
test = do
  print cmdConnect
  print cmdProducer
  print cmdSubscribe
