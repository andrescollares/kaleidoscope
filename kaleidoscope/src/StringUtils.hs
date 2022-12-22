module StringUtils where

import Data.Word
import qualified Data.ByteString as B
import qualified Data.ByteString.Short as B (ShortByteString, toShort, fromShort)

charToWord8 :: Char -> Word8
charToWord8 = toEnum . fromEnum

stringToShortByteString :: [Char] -> B.ShortByteString
stringToShortByteString s = B.toShort (B.pack $ map charToWord8 (show s))

shortByteStringToString :: B.ShortByteString -> [Char]
shortByteStringToString s = show $ B.unpack $ B.fromShort s

byteStringToString :: B.ByteString -> [Char]
byteStringToString s = show $ B.unpack s