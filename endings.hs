import Data.ByteString
import Data.Word
import Data.Bits

data Endian = Little | Big

padLeft::Int->Word8->ByteString->ByteString
padLeft targlen pad str 
                        | targlen<= len = str
                        | otherwise = append (replicate (targlen-len) pad) str
      where len = length str
padRight::Int->Word8->ByteString->ByteString
padRight targlen pad str 
                        | targlen<= len = str
                        | otherwise = append str (replicate (targlen-len) pad)
      where len = length str
lowestByte::Integral a => a->Maybe (Word8,a)
lowestByte x 
            | x /= 0 = Just ((fromIntegral x) .&. 255::Word8, shiftR x 8)
            | x == 0 = Nothing
encodeWord16::Endian->Word16->ByteString
encodeWord16 Little a = padRight 4 0 $ unfoldr lowestByte a  
