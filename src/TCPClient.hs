{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module TCPClient
  ( send
  )
where

import qualified Control.Exception             as E
import           Frame                          ( encodeBaseCommand )
import qualified Network.Socket                as NS
import qualified Network.Socket.ByteString     as BS
import           Proto.PulsarApi                ( BaseCommand )

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
