module Pulsar.Types where

import qualified Data.ByteString.Lazy.Char8    as CL
import           Data.Char                      ( toLower )
import           Data.String
import qualified Data.Text                     as T
import           Proto.PulsarApi                ( MessageIdData )

------- Topic ---------

data Topic = Topic
  { type' :: TopicType
  , tenant :: Tenant
  , namespace :: NameSpace
  , name :: TopicName
  }

defaultTopic :: String -> Topic
defaultTopic n = Topic { type'     = NonPersistent
                       , tenant    = Tenant "public"
                       , namespace = NameSpace "default"
                       , name      = TopicName n
                       }

instance Show Topic where
  show (Topic typ tn ns n) =
    toLower <$> show typ <> "://" <> show tn <> "/" <> show ns <> "/" <> show n

data TopicType = Persistent | NonPersistent

instance Show TopicType where
  show Persistent    = "persistent"
  show NonPersistent = "non-persistent"

newtype Tenant = Tenant String

instance Show Tenant where
  show (Tenant t) = t

newtype NameSpace = NameSpace String

instance Show NameSpace where
  show (NameSpace t) = t

newtype TopicName = TopicName String

instance Show TopicName where
  show (TopicName t) = t

newtype MsgId = MsgId MessageIdData

data Message = Message MsgId CL.ByteString

newtype PulsarMessage = PulsarMessage CL.ByteString deriving Show

instance IsString PulsarMessage where
  fromString = PulsarMessage . CL.pack

------- Subscription ---------

newtype SubscriptionName = SubscriptionName T.Text deriving Show

instance IsString SubscriptionName where
  fromString = SubscriptionName . T.pack
