module Hash (hash256, hexStringToByteString) where

import Crypto.Hash.SHA256 (hash)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Data.Char (digitToInt)

hash256 :: ByteString -> ByteString
hash256 = hash . hash

hexStringToByteString :: String -> ByteString
hexStringToByteString ('0' : 'x' : hexStr)
  | even (length hexStr) = pack (hexCharsToBytes hexStr)
  | otherwise = error "length must be even"
  where
    hexCharsToBytes [] = []
    hexCharsToBytes [_] = undefined
    hexCharsToBytes (a : b : rest) = toEnum (16 * digitToInt a + digitToInt b) : hexCharsToBytes rest
hexStringToByteString _ = error "prefix `0x` is missing"
