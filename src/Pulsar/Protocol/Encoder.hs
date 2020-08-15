{-# LANGUAGE OverloadedStrings #-}

module Pulsar.Protocol.Encoder
  ( encodeBaseCommand
  )
where

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
import           Pulsar.Protocol.Frame

mkSimpleCommand :: Int32 -> BaseCommand -> SimpleCmd
mkSimpleCommand extraBytes cmd = SimpleCommand
  { frameTotalSize   = cmdSize + extraBytes
  , frameCommandSize = cmdSize
  , frameMessage     = msg
  }
 where
  msg     = CL.fromStrict $ PL.encodeMessage cmd
  cmdSize = fromIntegral $ CL.length msg

mkPayloadCommand
  :: BaseCommand -> Metadata -> Payload -> (SimpleCmd, PayloadCmd)
mkPayloadCommand cmd meta (Payload pl) = (simpleCmd, payloadCmd)
 where
  -- payload fields
  metadata    = either PL.encodeMessage PL.encodeMessage meta
  metaSize    = fromIntegral . C.length $ metadata
  metaSizeBS  = B.runPut . B.putInt32be $ metaSize
  checksum    = crc32c $ CL.toStrict metaSizeBS <> metadata <> CL.toStrict pl
  payloadSize = fromIntegral . CL.length $ pl
  -- simple fields
  message     = CL.fromStrict . PL.encodeMessage $ cmd
  cmdSize     = fromIntegral . CL.length $ message
  -- frame: extra 14 bytes = 2 (magic number) + 4 (checksum) + 4 (metadata size) + 4 (command size)
  extraBytes  = fromIntegral (14 + metaSize) + payloadSize
  simpleCmd   = mkSimpleCommand extraBytes cmd
  payloadCmd  = PayloadCommand { frameMagicNumber  = 0x0e01
                               , frameCheckSum     = checksum
                               , frameMetadataSize = metaSizeBS
                               , frameMetadata     = CL.fromStrict metadata
                               , framePayload      = pl
                               }

encodeSimpleCmd :: SimpleCmd -> C.ByteString
encodeSimpleCmd (SimpleCommand ts cs msg) =
  let totalSize   = B.runPut $ B.putInt32be ts
      commandSize = B.runPut $ B.putInt32be cs
  in  CL.toStrict $ totalSize <> commandSize <> msg

encodeFrame :: Frame -> C.ByteString
encodeFrame (SimpleFrame scmd) = encodeSimpleCmd scmd
encodeFrame (PayloadFrame scmd (PayloadCommand mn cs mds md p)) =
  let simpleCmd   = CL.fromStrict $ encodeSimpleCmd scmd
      magicNumber = B.runPut $ B.putWord16be mn
      crc32cSum   = B.runPut $ B.putWord32be cs
  in  CL.toStrict $ simpleCmd <> magicNumber <> crc32cSum <> mds <> md <> p

encodeBaseCommand
  :: Maybe Metadata -> Maybe Payload -> BaseCommand -> C.ByteString
encodeBaseCommand (Just meta) p cmd =
  let pl = fromMaybe (Payload "") p
  in  encodeFrame . uncurry PayloadFrame $ mkPayloadCommand cmd meta pl
encodeBaseCommand Nothing _ cmd =
  encodeFrame . SimpleFrame . (mkSimpleCommand 4) $ cmd
