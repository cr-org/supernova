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

-- Simple command: http://pulsar.apache.org/docs/en/develop-binary-protocol/#simple-commands
data Frame = Frame
  { commandSize :: Int32          -- The size of the protobuf-serialized command
  , totalSize :: Int32            -- The size of the frame, counting everything that comes after it (in bytes)
  , baseCommand :: CL.ByteString  -- The protobuf message serialized in a raw binary format (rather than in protobuf format)
  }

mkFrame :: BaseCommand -> Frame
mkFrame cmd = Frame { commandSize = cmdSize
                    , totalSize   = cmdSize + 4
                    , baseCommand = message
                    }
 where
  message = CL.fromStrict $ PL.encodeMessage cmd
  cmdSize = fromIntegral $ CL.length message

encodeFrame :: Frame -> C.ByteString
encodeFrame (Frame cs ts base) =
  let totalSize   = B.runPut $ B.putInt32be ts
      commandSize = B.runPut $ B.putInt32be cs
  in  BL.toStrict $ totalSize <> commandSize <> base

encodeBaseCommand :: BaseCommand -> C.ByteString
encodeBaseCommand = encodeFrame . mkFrame

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
