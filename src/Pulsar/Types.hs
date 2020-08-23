{-|
Module      : Pulsar.Types
Description : End-user Pulsar API types.
License     : Apache-2.0
Maintainer  : gabriel.volpe@chatroulette.com
Stability   : experimental

End-user types to configure consumers and producers.
-}
module Pulsar.Types where

import qualified Data.ByteString.Lazy.Char8    as CL
import           Data.Char                      ( toLower )
import           Data.String
import qualified Data.Text                     as T
import           Proto.PulsarApi                ( MessageIdData )

{- | A Topic is in the form "type:\/\/tenant\/namespace\/topic-name", which is what the 'Show' instance does. -}
data Topic = Topic
  { type' :: TopicType
  , tenant :: Tenant
  , namespace :: NameSpace
  , name :: TopicName
  }

{- | A default 'Topic': "non-persistent:\/\/public\/default\/my-topic". -}
defaultTopic :: String -> Topic
defaultTopic n = Topic { type'     = NonPersistent
                       , tenant    = Tenant "public"
                       , namespace = NameSpace "default"
                       , name      = TopicName n
                       }

instance Show Topic where
  show (Topic typ tn ns n) =
    toLower <$> show typ <> "://" <> show tn <> "/" <> show ns <> "/" <> show n

{- | A topic can be either 'Persistent' or 'NonPersistent'. -}
data TopicType = Persistent | NonPersistent

instance Show TopicType where
  show Persistent    = "persistent"
  show NonPersistent = "non-persistent"

{- | A tenant can be any string value. Default value is "public". -}
newtype Tenant = Tenant String

instance Show Tenant where
  show (Tenant t) = t

{- | A namespace can be any string value. Default value is "default". -}
newtype NameSpace = NameSpace String

instance Show NameSpace where
  show (NameSpace t) = t

{- | A topic name can be any string value. -}
newtype TopicName = TopicName String

instance Show TopicName where
  show (TopicName t) = t

{- | A message id, needed for acknowledging messages. See 'Pulsar.Consumer.ack'. -}
newtype MsgId = MsgId MessageIdData

{- | A consumed message, containing both 'MsgId' and payload as bytestring. -}
data Message = Message MsgId CL.ByteString

{- | A produced message, containing just a payload as bytestring. -}
newtype PulsarMessage = PulsarMessage CL.ByteString deriving Show

instance IsString PulsarMessage where
  fromString = PulsarMessage . CL.pack

{- | A subscription name can be any string value. -}
newtype SubscriptionName = SubscriptionName T.Text deriving Show

instance IsString SubscriptionName where
  fromString = SubscriptionName . T.pack
