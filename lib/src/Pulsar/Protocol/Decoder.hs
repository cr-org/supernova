{-# LANGUAGE FlexibleInstances, LambdaCase, OverloadedStrings #-}

{- A decoder that understands the Pulsar protocol, as specified at: http://pulsar.apache.org/docs/en/develop-binary-protocol -}
module Pulsar.Protocol.Decoder
  ( decodeBaseCommand
  , dropPayloadGarbage
  )
where

import           Control.Monad                  ( guard )
import qualified Data.Binary                   as B
import qualified Data.Binary.Get               as B
import qualified Data.Binary.Put               as B
import qualified Data.ByteString.Lazy.Char8    as CL
import           Data.Bifunctor                 ( bimap )
import           Data.Int                       ( Int32 )
import qualified Data.ProtoLens.Encoding       as PL
import           Pulsar.Protocol.CheckSum
import           Pulsar.Protocol.Frame

data ValidateCheckSum = Yes | No deriving Show

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

{- | Parse total size, command size and message. If done, return simple frame. Otherwise, try to parse a payload frame. -}
parseFrame :: B.Get Frame
parseFrame = do
  ts <- B.getInt32be
  cs <- B.getInt32be
  ms <- B.getLazyByteString (fromIntegral cs)
  let simpleCmd  = SimpleCommand ts cs ms
      payloadRes = parsePayload ts cs simpleCmd
  B.isEmpty >>= \case
    True  -> return $! SimpleFrame simpleCmd
    False -> validateMagicNumber payloadRes

{- | The 2-bytes "magic number" is optional. If present, it indicates that a 4-bytes checksum follows. -}
validateMagicNumber :: (ValidateCheckSum -> B.Get Frame) -> B.Get Frame
validateMagicNumber payload = B.lookAheadM peekMagicNumber >>= \case
  Just _  -> payload Yes
  Nothing -> payload No
 where
  peekMagicNumber :: B.Get (Maybe ())
  peekMagicNumber = guard . (== frameMagicNumber) <$> B.getWord16be

{- | If a checksum is given, validate it. Otherwise, return simple frame. -}
validateCheckSum :: Frame -> B.Get Frame
validateCheckSum (PayloadFrame sc (PayloadCommand cs@(Just csm) ms md pl)) =
  case runCheckSum (B.runPut (B.putInt32be ms) <> md <> pl) csm of
    Valid -> return
      $! PayloadFrame sc (PayloadCommand cs ms md (dropPayloadGarbage pl))
    Invalid -> fail "Invalid checksum"
validateCheckSum x = return $! x

{- | Take in a simple command and try to parse a payload command. -}
parsePayload :: Int32 -> Int32 -> SimpleCmd -> ValidateCheckSum -> B.Get Frame
parsePayload ts cs simpleCmd vcs = case vcs of
  Yes -> parsePayload' . Just . CheckSum =<< B.getWord32be
  No  -> parsePayload' Nothing
 where
  parsePayload' cm = do
    ms <- B.getInt32be
    md <- B.getLazyByteString . fromIntegral $ ms
    pl <- B.getLazyByteString . fromIntegral $ ts - (remaining cm + cs + ms)
    let frame = PayloadFrame simpleCmd (PayloadCommand cm ms md pl)
    validateCheckSum frame
  remaining (Just _) = 14 -- 4 (command size) + 2 (magic number) + 4 (checksum) + 4 (metadata size)
  remaining Nothing  = 8  -- no magic number and checksum

decodeFrame :: CL.ByteString -> Either String Frame
decodeFrame =
  bimap (\(_, _, e) -> e) (\(_, _, f) -> f) . B.runGetOrFail parseFrame

{- | Decode either a 'SimpleFrame' or a 'PayloadFrame'. -}
decodeBaseCommand :: CL.ByteString -> Either String Response
decodeBaseCommand bytes = decodeFrame bytes >>= \case
  SimpleFrame s ->
    SimpleResponse <$> PL.decodeMessage (CL.toStrict $ frameMessage s)
  PayloadFrame s (PayloadCommand _ _ md pl) -> do
    cmd  <- PL.decodeMessage . CL.toStrict $ frameMessage s
    meta <- PL.decodeMessage . CL.toStrict $ md
    return $ PayloadResponse cmd meta (payload pl)
   where
    payload p | CL.null p = Nothing
              | otherwise = Just $ Payload p
