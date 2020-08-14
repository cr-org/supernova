{-# LANGUAGE OverloadedStrings, RankNTypes #-}

module Pulsar.Internal.Frame
  ( Metadata(..)
  , Payload(..)
  , encodeBaseCommand
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
import           Data.Maybe                     ( fromMaybe )
import qualified Data.ProtoLens.Encoding       as PL
import qualified Data.ProtoLens.Encoding.Bytes as PL
import           Lens.Family
import           Proto.PulsarApi                ( BaseCommand
                                                , SingleMessageMetadata
                                                , MessageMetadata
                                                )

{- MaxFrameSize is defined by the Pulsar spec with a single
 - sentence: "The maximum allowable size of a single frame is 5 MB."
 -
 - http://pulsar.apache.org/docs/en/develop-binary-protocol/#framing
 -}
--maxFrameSize = 5 * 1024 * 1024 -- 5mb

data Frame = SimpleFrame SimpleCmd | PayloadFrame SimpleCmd PayloadCmd

-- Simple command: http://pulsar.apache.org/docs/en/develop-binary-protocol/#simple-commands
data SimpleCmd = SimpleCommand
  { totalSize :: Int32        -- The size of the frame, counting everything that comes after it (in bytes)
  , commandSize :: Int32      -- The size of the protobuf-serialized command
  , message :: CL.ByteString  -- The protobuf message serialized in a raw binary format (rather than in protobuf format)
  }

-- Payload command: http://pulsar.apache.org/docs/en/develop-binary-protocol/#payload-commands
data PayloadCmd = PayloadCommand
  { magicNumber :: B.Word16         -- A 2-byte byte array (0x0e01) identifying the current format
  , checkSum :: B.Word32            -- A CRC32-C checksum of everything that comes after it
  , metadataSize :: CL.ByteString   -- The size of the message metadata
  , metadata :: CL.ByteString       -- The message metadata stored as a binary protobuf message
  , payload :: CL.ByteString        -- Anything left in the frame is considered the payload and can include any sequence of bytes
  }

data Metadata = Single SingleMessageMetadata | Multi MessageMetadata

newtype Payload = Payload CL.ByteString

mkSimpleCommand :: BaseCommand -> SimpleCmd
mkSimpleCommand cmd = SimpleCommand { totalSize   = cmdSize + 4
                                    , commandSize = cmdSize
                                    , message     = msg
                                    }
 where
  msg     = CL.fromStrict $ PL.encodeMessage cmd
  cmdSize = fromIntegral $ CL.length msg

mkPayloadCommand
  :: BaseCommand -> Metadata -> Payload -> (SimpleCmd, PayloadCmd)
mkPayloadCommand cmd (Single meta) (Payload pl) = (simpleCmd, payloadCmd)
 where
  simpleCmd  = mkSimpleCommand cmd
  md         = PL.encodeMessage meta
  mds        = B.runPut . B.putInt32be $ fromIntegral (C.length md)
  cs         = crc32c $ CL.toStrict mds <> md <> CL.toStrict pl
  payloadCmd = PayloadCommand { magicNumber  = 0x0e01
                              , checkSum     = cs
                              , metadataSize = mds
                              , metadata     = CL.fromStrict md
                              , payload      = pl
                              }
mkPayloadCommand cmd (Multi meta) (Payload pl) = undefined -- TODO

encodeSimpleCmd :: SimpleCmd -> C.ByteString
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
  in  BL.toStrict $ simpleCmd <> magicNr <> crc32cSum <> mds <> md <> p

encodeBaseCommand
  :: Maybe Metadata -> Maybe Payload -> BaseCommand -> C.ByteString
encodeBaseCommand (Just meta) p cmd =
  let pl = fromMaybe (Payload "") p
  in  encodeFrame . uncurry PayloadFrame $ mkPayloadCommand cmd meta pl
encodeBaseCommand Nothing _ cmd =
  encodeFrame . SimpleFrame . mkSimpleCommand $ cmd
