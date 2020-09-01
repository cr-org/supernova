module Pulsar.Protocol.CheckSum where

import qualified Data.Binary                   as B
import           Data.Bool                      ( bool )
import qualified Data.ByteString.Lazy.Char8    as CL
import           Data.Digest.CRC32C             ( crc32c )

data CheckSumValidation = Valid | Invalid deriving Show

newtype CheckSum = CheckSum B.Word32 deriving (Eq, Show)

runCheckSum :: CL.ByteString -> CheckSum -> CheckSumValidation
runCheckSum t cs = bool Invalid Valid $ computeCheckSum t == cs

computeCheckSum :: CL.ByteString -> CheckSum
computeCheckSum t = CheckSum $ crc32c (CL.toStrict t)
