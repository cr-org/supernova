{- An encoder that understands the Pulsar protocol, as specified at: http://pulsar.apache.org/docs/en/develop-binary-protocol -}
module Pulsar.Protocol.Encoder
  ( encodeBaseCommand
  )
where

import qualified Data.Binary.Put               as B
import qualified Data.ByteString.Lazy.Char8    as CL
import           Data.Int                       ( Int32 )
import           Data.Maybe                     ( fromMaybe )
import qualified Data.ProtoLens.Encoding       as PL
import           Proto.PulsarApi                ( BaseCommand
                                                , MessageMetadata
                                                )
import           Pulsar.Protocol.CheckSum
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
  :: BaseCommand -> MessageMetadata -> Payload -> (SimpleCmd, PayloadCmd)
mkPayloadCommand cmd meta (Payload pl) = (simpleCmd, payloadCmd)
 where
  -- payload fields
  metadata    = PL.encodeMessage meta
  metaSize    = fromIntegral . CL.length . CL.fromStrict $ metadata
  metaSizeBS  = B.runPut $ B.putInt32be metaSize
  checkSum    = computeCheckSum $ metaSizeBS <> CL.fromStrict metadata <> pl
  payloadSize = fromIntegral $ CL.length pl
  -- frame: extra 14 bytes = 2 (magic number) + 4 (checksum) + 4 (metadata size) + 4 (command size)
  extraBytes  = fromIntegral (14 + metaSize) + payloadSize
  simpleCmd   = mkSimpleCommand extraBytes cmd
  payloadCmd  = PayloadCommand { frameCheckSum     = Just checkSum
                               , frameMetadataSize = metaSize
                               , frameMetadata     = CL.fromStrict metadata
                               , framePayload      = pl
                               }

encodeSimpleCmd :: SimpleCmd -> CL.ByteString
encodeSimpleCmd (SimpleCommand ts cs msg) =
  let totalSize   = B.runPut $ B.putInt32be ts
      commandSize = B.runPut $ B.putInt32be cs
  in  totalSize <> commandSize <> msg

encodeFrame :: Frame -> CL.ByteString
encodeFrame (SimpleFrame scmd) = encodeSimpleCmd scmd
encodeFrame (PayloadFrame scmd (PayloadCommand cs mds md p)) =
  let simpleCmd  = encodeSimpleCmd scmd
      metaSizeBS = B.runPut $ B.putInt32be mds
      payloadCmd = encodeOptionalFields cs <> metaSizeBS <> md <> p
  in  simpleCmd <> payloadCmd

-- If a magic number is present, a CRC32-C checksum of everything that comes after it (4 bytes) should follow
encodeOptionalFields :: Maybe CheckSum -> CL.ByteString
encodeOptionalFields (Just (CheckSum cs)) =
  let magicNumber = B.runPut $ B.putWord16be frameMagicNumber
      crc32cSum   = B.runPut $ B.putWord32be cs
  in  magicNumber <> crc32cSum
encodeOptionalFields Nothing = CL.empty

encodeBaseCommand
  :: Maybe MessageMetadata -> Maybe Payload -> BaseCommand -> CL.ByteString
encodeBaseCommand (Just meta) p cmd =
  let pl = fromMaybe (Payload CL.empty) p
  in  encodeFrame . uncurry PayloadFrame $ mkPayloadCommand cmd meta pl
encodeBaseCommand Nothing _ cmd =
  encodeFrame . SimpleFrame . mkSimpleCommand 4 $ cmd
