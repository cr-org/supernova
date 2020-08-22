{-# LANGUAGE FlexibleInstances, LambdaCase, OverloadedStrings #-}

{- A decoder that understands the Pulsar protocol, as specified at: http://pulsar.apache.org/docs/en/develop-binary-protocol -}
module Pulsar.Protocol.Decoder
  ( decodeBaseCommand
  , dropPayloadGarbage
  )
where

import           Control.Monad                  ( unless )
import qualified Data.Binary.Get               as B
import qualified Data.Binary.Put               as B
import qualified Data.ByteString.Lazy.Char8    as CL
import           Data.Digest.CRC32C             ( crc32c )
import           Data.Bifunctor                 ( bimap )
import           Data.Int                       ( Int32 )
import qualified Data.ProtoLens.Encoding       as PL
import           Proto.PulsarApi                ( BaseCommand )
import           Pulsar.Protocol.Frame

{-
 - These 5 bytes are part of a total of 8 bytes sent as the payload's prefix from the Java client.
 - Apparently that's how Google's FlatBuffers serialize data: https://google.github.io/flatbuffers/
 -
 - Source: https://github.com/apache/pulsar/blob/master/pulsar-io/kinesis/src/main/java/org/apache/pulsar/io/kinesis/fbs/Message.java#L22
 -
 - More info on the Ascii spec: https://www.december.com/html/spec/ascii.html. Maybe this could be helpful: https://hackage.haskell.org/package/flatbuffers
 -}
dropPayloadGarbage :: CL.ByteString -> CL.ByteString
dropPayloadGarbage bs =
  maybe bs (CL.drop 3) (CL.stripPrefix "\NUL\NUL\NUL\EOT\CAN" bs)

parseFrame :: B.Get Frame
parseFrame = do
  ts <- B.getInt32be
  cs <- B.getInt32be
  ms <- B.getLazyByteString (fromIntegral cs)
  let simpleCmd = SimpleCommand ts cs ms
  B.isEmpty >>= \case
    True  -> return $ SimpleFrame simpleCmd
    False -> parsePayload ts cs simpleCmd

validateCheckSum :: Frame -> B.Get Frame
validateCheckSum frame@(PayloadFrame sc (PayloadCommand cs ms md pl)) =
  let
    metaSize = CL.toStrict (B.runPut $ B.putInt32be ms)
    metadata = CL.toStrict md
    payload  = CL.toStrict pl
    checksum = crc32c $ metaSize <> metadata <> payload
    newFrame =
      PayloadFrame sc (PayloadCommand cs ms md (dropPayloadGarbage pl))
  in
    if checksum == cs then return $! newFrame else fail "Invalid checksum"
validateCheckSum x = return $! x

parsePayload :: Int32 -> Int32 -> SimpleCmd -> B.Get Frame
parsePayload ts cs simpleCmd = do
  mn <- B.getWord16be
  unless (mn == frameMagicNumber) $ fail ("Invalid magic number: " <> show mn)
  cm <- B.getWord32be
  ms <- B.getInt32be
  md <- B.getLazyByteString . fromIntegral $ ms
  -- 14 remaining bytes = 4 (command size field) + 2 (magic number) + 4 (checksum) + 4 (metadata size field)
  pl <- payload $ ts - (14 + cs + ms)
  let payloadCmd = PayloadCommand cm ms md pl
  validateCheckSum (PayloadFrame simpleCmd payloadCmd)
 where
  payload rms | rms > 0   = B.getLazyByteString $ fromIntegral rms
              | otherwise = pure CL.empty

decodeFrame :: CL.ByteString -> Either String Frame
decodeFrame =
  bimap (\(_, _, e) -> e) (\(_, _, f) -> f) . B.runGetOrFail parseFrame

decodeBaseCommand :: CL.ByteString -> Either String Response
decodeBaseCommand bytes = decodeFrame bytes >>= \case
  SimpleFrame s -> do
    cmd <- PL.decodeMessage (CL.toStrict $ frameMessage s)
    return $ SimpleResponse cmd
  PayloadFrame s (PayloadCommand cs ms md pl) -> do
    cmd  <- PL.decodeMessage . CL.toStrict $ frameMessage s
    meta <- PL.decodeMessage . CL.toStrict $ md
    return $ PayloadResponse cmd meta (payload pl)
   where
    payload pl | CL.null pl = Nothing
               | otherwise  = Just $ Payload pl
