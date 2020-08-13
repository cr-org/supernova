{-# LANGUAGE RankNTypes #-}

module Frame
  ( encodeBaseCommand
  )
where

import qualified Data.Binary                   as B
import qualified Data.Binary.Put               as B
import qualified Data.ByteString.Lazy          as BL
import qualified Data.ByteString.Char8         as C
import qualified Data.ByteString.Lazy.Char8    as CL
import           Data.Int                       ( Int32 )
import           Data.Maybe                     ( isNothing )
import qualified Data.ProtoLens.Encoding       as PL
import qualified Data.ProtoLens.Encoding.Bytes as PL
import           Lens.Family
import           Proto.PulsarApi                ( BaseCommand )
import qualified Proto.PulsarApi_Fields        as F

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
  { magicNumber :: Int32            -- A 2-byte byte array (0x0e01) identifying the current format
  , checkSum :: Int32               -- A CRC32-C checksum of everything that comes after it
  , metadataSize :: Int32           -- The size of the message metadata
  , metadata :: CL.ByteString       -- The message metadata stored as a binary protobuf message
  , payload :: CL.ByteString        -- Anything left in the frame is considered the payload and can include any sequence of bytes
  }

mkSimpleCommand :: BaseCommand -> Simple
mkSimpleCommand cmd =
  SimpleCommand { totalSize   = cmdSize + 4
                , commandSize = cmdSize
                , message     = msg
                }
 where
  msg     = CL.fromStrict $ PL.encodeMessage cmd
  cmdSize = fromIntegral $ CL.length msg

mkPayloadCommand :: BaseCommand -> (Simple, Payload)
mkPayloadCommand cmd =
  let simple  = mkSimpleCommand cmd
      payload = undefined
  in  (simple, payload)

encodeFrame :: Frame -> C.ByteString
encodeFrame (SimpleFrame (SimpleCommand ts cs base)) =
  let totalSize   = B.runPut $ B.putInt32be ts
      commandSize = B.runPut $ B.putInt32be cs
  in  BL.toStrict $ totalSize <> commandSize <> base
encodeFrame (PayloadFrame _ _) = undefined

encodeBaseCommand :: BaseCommand -> C.ByteString
encodeBaseCommand cmd =
  let sf = encodeFrame . SimpleFrame . mkSimpleCommand
      pf = encodeFrame . (uncurry PayloadFrame) . mkPayloadCommand
  in  if isSimple cmd then sf cmd else pf cmd

isSimple :: BaseCommand -> Bool
isSimple cmd =
  _bool F.maybe'producer F.vec'metadata cmd
    && _bool F.maybe'subscribe F.vec'metadata cmd

_bool
  :: forall a b c
   . LensLike' (Constant (Maybe a)) c (Maybe a)
  -> LensLike' (Constant (First b)) a b
  -> c
  -> Bool
_bool ls1 ls2 cmd = isNothing ((^? ls2) =<< cmd ^. ls1)
