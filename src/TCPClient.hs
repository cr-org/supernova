{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module TCPClient
  ( send
  )
where

import qualified Control.Exception             as E
import qualified Data.ByteString.Char8         as C
import           Data.ProtoLens.Encoding        ( buildMessageDelimited
                                                , encodeMessage
                                                )
import           Data.ProtoLens.Encoding.Bytes  ( runBuilder )
import           Network.Socket
import           Network.Socket.ByteString      ( recv
                                                , sendAll
                                                )
import           Proto.PulsarApi                ( BaseCommand )

--Got exception TooLongFrameException : Adjusted frame length exceeds 5253120: 134353446 - discarded
--pulsar_1          | io.netty.handler.codec.TooLongFrameException: Adjusted frame length exceeds 5253120: 134353446 - discarded
--
--TODO: it seems we need to set the socket's output buffer to 5253120 or lower -> http://pulsar.apache.org/docs/en/develop-binary-protocol/#framing
send :: BaseCommand -> IO ()
send cmd = runTCPClient "127.0.0.1" "6650" $ \s -> do
  --sendAll s $ runBuilder (buildMessageDelimited cmd)
  sendAll s $ encodeMessage cmd
  msg <- recv s 4096
  C.putStrLn $ "Received: " <> msg

runTCPClient :: HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPClient host port client = withSocketsDo $ do
  addr <- resolve
  E.bracket (open addr) close client
 where
  resolve = do
    let hints = defaultHints { addrSocketType = Stream }
    getAddrInfo (Just hints) (Just host) (Just port) >>= \case
      [addr] -> pure addr
      _      -> E.ioError $ userError "Could not resolve socket address"
  open addr = E.bracketOnError (openSocket addr) close $ \sock -> do
    connect sock $ addrAddress addr
    return sock
