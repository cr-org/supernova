{-# LANGUAGE FlexibleInstances, LambdaCase #-}

module Pulsar.Protocol.Decoder where

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
validateCheckSum frame@(PayloadFrame _ (PayloadCommand cs ms md pl)) =
  let metaSize = CL.toStrict (B.runPut $ B.putInt32be ms)
      metadata = CL.toStrict md
      payload  = CL.toStrict pl
      checksum = crc32c $ metaSize <> metadata <> payload
  in  if checksum == cs then return $! frame else fail "Invalid checksum"
validateCheckSum x = return $! x

parsePayload :: Int32 -> Int32 -> SimpleCmd -> B.Get Frame
parsePayload ts cs simpleCmd = do
  mn <- B.getWord16be
  unless (mn == frameMagicNumber) $ fail ("Invalid magic number: " <> show mn)
  cm <- B.getWord32be
  ms <- B.getInt32be
  md <- B.getLazyByteString . fromIntegral $ ms
  -- 10 bytes = 2 (checksum) + 4 (metadata size) + 4 (metadata)
  let remainingBytes = ts - (10 + ms + cs)
  pl <- payload remainingBytes
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
    return $ PayloadResponse cmd (Right meta) (payload pl)
   where
    payload pl | CL.null pl = Nothing
               | otherwise  = Just $ Payload pl
