{-# LANGUAGE OverloadedStrings, RankNTypes #-}

module Pulsar.Internal.Frame
  ( encodeBaseCommand
  )
where

import           Control.Applicative            ( (<|>) )
import qualified Data.Binary                   as B
import qualified Data.Binary.Put               as B
import qualified Data.ByteString.Lazy          as BL
import qualified Data.ByteString.Char8         as C
import qualified Data.ByteString.Lazy.Char8    as CL
import           Data.Digest.CRC32C             ( crc32c )
import           Data.Int                       ( Int32 )
import           Data.Maybe                     ( fromMaybe
                                                , isNothing
                                                )
import qualified Data.ProtoLens.Encoding       as PL
import qualified Data.ProtoLens.Encoding.Bytes as PL
import           Lens.Family
import           Proto.PulsarApi
import qualified Proto.PulsarApi_Fields        as F
import qualified Data.ProtoLens.Runtime.Data.Vector
                                               as Data.Vector
import           Data.ProtoLens.Field           ( HasField )

{- MaxFrameSize is defined by the Pulsar spec with a single
 - sentence: "The maximum allowable size of a single frame is 5 MB."
 -
 - http://pulsar.apache.org/docs/en/develop-binary-protocol/#framing
 -}
--maxFrameSize = 5 * 1024 * 1024 -- 5mb

data Frame = SimpleFrame Simple | PayloadFrame Simple Payload

-- Simple command: http://pulsar.apache.org/docs/en/develop-binary-protocol/#simple-commands
data Simple = SimpleCommand
  { totalSize :: Int32        -- The size of the frame, counting everything that comes after it (in bytes)
  , commandSize :: Int32      -- The size of the protobuf-serialized command
  , message :: CL.ByteString  -- The protobuf message serialized in a raw binary format (rather than in protobuf format)
  }

-- Payload command: http://pulsar.apache.org/docs/en/develop-binary-protocol/#payload-commands
data Payload = PayloadCommand
  { magicNumber :: B.Word16         -- A 2-byte byte array (0x0e01) identifying the current format
  , checkSum :: B.Word32            -- A CRC32-C checksum of everything that comes after it
  , metadataSize :: CL.ByteString   -- The size of the message metadata
  , metadata :: CL.ByteString       -- The message metadata stored as a binary protobuf message
  , payload :: Maybe CL.ByteString  -- Anything left in the frame is considered the payload and can include any sequence of bytes
  }

mkSimpleCommand :: BaseCommand -> Simple
mkSimpleCommand cmd = SimpleCommand { totalSize   = cmdSize + 4
                                    , commandSize = cmdSize
                                    , message     = msg
                                    }
 where
  msg     = CL.fromStrict $ PL.encodeMessage cmd
  cmdSize = fromIntegral $ CL.length msg

type Metadata = Data.Vector.Vector KeyValue

mkPayloadCommand :: BaseCommand -> MessageMetadata -> (Simple, Payload)
mkPayloadCommand cmd msgMd = (simpleCmd, payloadCmd)
 where
  simpleCmd  = mkSimpleCommand cmd
  md         = PL.encodeMessage msgMd
  mds        = B.runPut . B.putInt32be $ fromIntegral (C.length md)
  cs         = crc32c $ md <> CL.toStrict mds
  payloadCmd = PayloadCommand { magicNumber  = 0x0e01
                              , checkSum     = cs
                              , metadataSize = mds
                              , metadata     = CL.fromStrict md
                              , payload      = Nothing
                              }

encodeSimpleCmd :: Simple -> C.ByteString
encodeSimpleCmd (SimpleCommand ts cs msg) =
  let totalSize   = B.runPut $ B.putInt32be ts
      commandSize = B.runPut $ B.putInt32be cs
  in  BL.toStrict $ totalSize <> commandSize <> msg

encodeFrame :: Frame -> C.ByteString
encodeFrame (SimpleFrame scmd) = encodeSimpleCmd scmd
encodeFrame (PayloadFrame scmd (PayloadCommand mn cs mds md p)) =
  let simpleCmd = CL.fromStrict $ encodeSimpleCmd scmd
      magicNr   = B.runPut $ B.putWord16be mn
      crc32cSum = B.runPut $ B.putWord32be cs
      bPayload  = fromMaybe "" p
  in  BL.toStrict $ simpleCmd <> magicNr <> crc32cSum <> mds <> md <> bPayload

encodeBaseCommand :: BaseCommand -> C.ByteString
encodeBaseCommand cmd = case getMetadata cmd of
  Nothing -> encodeFrame . SimpleFrame . mkSimpleCommand $ cmd
  Just md -> encodeFrame . uncurry PayloadFrame $ mkPayloadCommand cmd md

-- Only CommandProducer and CommandSubribe contain metadata. These are considered PayloadCommands.
getMetadata :: BaseCommand -> Maybe MessageMetadata
getMetadata cmd =
  let p = _just F.maybe'producer F.metadata cmd
      s = _just F.maybe'subscribe F.metadata cmd
  in  Nothing -- p <|> s

_just
  :: forall a b c
   . LensLike' (Constant (Maybe a)) c (Maybe a)
  -> LensLike' (Constant (First b)) a b
  -> c
  -> Maybe b
_just ls1 ls2 cmd = (^? ls2) =<< cmd ^. ls1
