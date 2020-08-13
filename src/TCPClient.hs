{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module TCPClient
  ( send
  )
where

import qualified Control.Exception             as E
import qualified Data.Binary                   as B
import qualified Data.Binary.Put               as B
import qualified Data.ByteString.Lazy          as BL
import qualified Data.ByteString.Char8         as C
import qualified Data.ByteString.Lazy.Char8    as CL
import           Data.Int                       ( Int32 )
import qualified Data.ProtoLens.Encoding       as PL
import qualified Data.ProtoLens.Encoding.Bytes as PL
import qualified Network.Socket                as NS
import qualified Network.Socket.ByteString     as BS
import           Proto.PulsarApi                ( BaseCommand )

{- MaxFrameSize is defined by the Pulsar spec with a single
 - sentence: "The maximum allowable size of a single frame is 5 MB."
 -
 - http://pulsar.apache.org/docs/en/develop-binary-protocol/#framing
 -}
--maxFrameSize = 5 * 1024 * 1024 -- 5mb

data Frame = SimpleFrame SimpleCommand | PayloadFrame PayloadCommand

-- Simple command: http://pulsar.apache.org/docs/en/develop-binary-protocol/#simple-commands
data SimpleCommand = SimpleCommand
  { totalSize :: Int32        -- The size of the frame, counting everything that comes after it (in bytes)
  , commandSize :: Int32      -- The size of the protobuf-serialized command
  , message :: CL.ByteString  -- The protobuf message serialized in a raw binary format (rather than in protobuf format)
  }

-- Payload command: http://pulsar.apache.org/docs/en/develop-binary-protocol/#payload-commands
data PayloadCommand = PayloadCommand
  { simpleCommand :: SimpleCommand  -- Total size, command size and message
  , magicNumber :: Int32            -- A 2-byte byte array (0x0e01) identifying the current format
  , checkSum :: Int32               -- A CRC32-C checksum of everything that comes after it
  , metadataSize :: Int32           -- The size of the message metadata
  , metadata :: CL.ByteString       -- The message metadata stored as a binary protobuf message
  , payload :: CL.ByteString        -- Anything left in the frame is considered the payload and can include any sequence of bytes
  }

mkSimpleFrame :: BaseCommand -> Frame
mkSimpleFrame cmd = SimpleFrame $
  SimpleCommand { totalSize   = cmdSize + 4
                , commandSize = cmdSize
                , message     = msg
                }
 where
  msg     = CL.fromStrict $ PL.encodeMessage cmd
  cmdSize = fromIntegral $ CL.length msg

encodeFrame :: Frame -> C.ByteString
encodeFrame (SimpleFrame (SimpleCommand ts cs base)) =
  let totalSize   = B.runPut $ B.putInt32be ts
      commandSize = B.runPut $ B.putInt32be cs
  in  BL.toStrict $ totalSize <> commandSize <> base
encodeFrame (PayloadFrame _) = undefined

encodeBaseCommand :: BaseCommand -> C.ByteString
encodeBaseCommand = encodeFrame . mkSimpleFrame

send :: BaseCommand -> IO ()
send cmd = runTCPClient "127.0.0.1" "6650" $ \s -> do
  BS.sendAll s $ encodeBaseCommand cmd
  msg <- BS.recv s 4096
  print $ "Received: " <> msg

runTCPClient :: NS.HostName -> NS.ServiceName -> (NS.Socket -> IO a) -> IO a
runTCPClient host port client = NS.withSocketsDo $ do
  addr <- resolve
  E.bracket (open addr) NS.close client
 where
  resolve = do
    let hints = NS.defaultHints { NS.addrSocketType = NS.Stream }
    NS.getAddrInfo (Just hints) (Just host) (Just port) >>= \case
      [addr] -> pure addr
      _      -> E.ioError $ userError "Could not resolve socket address"
  open addr = E.bracketOnError (NS.openSocket addr) NS.close $ \sock -> do
    NS.connect sock $ NS.addrAddress addr
    return sock
