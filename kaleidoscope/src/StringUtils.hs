module StringUtils where

import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString as BS
import Data.Word


stringToShortByteString :: String -> SBS.ShortByteString
stringToShortByteString str = SBS.pack (map (fromIntegral . fromEnum) str)

shortByteStringToString :: SBS.ShortByteString -> String
shortByteStringToString sbs = map (toEnum . fromIntegral) (SBS.unpack sbs)

-- charToWord8 :: Char -> Word8
-- charToWord8 c = fromIntegral (fromEnum c)

byteStringToString :: BS.ByteString -> String
byteStringToString bs = map (toEnum . fromIntegral) (BS.unpack bs)