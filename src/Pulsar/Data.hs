module Pulsar.Data where

import           Data.Char                      ( toLower )

data Topic = Topic
  { type' :: TopicType
  , tenant :: Tenant
  , namespace :: NameSpace
  , name :: TopicName
  }

defaultTopic :: String -> Topic
defaultTopic n = Topic { type'     = Persistent
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
