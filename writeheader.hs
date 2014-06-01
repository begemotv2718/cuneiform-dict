import Text.Printf
import Data.ByteString.Char8 as B
import Data.Word
import Data.Binary.Put
import Data.ByteString.Lazy as L hiding (pack)

--writeheader::Int->Int->Int->Int->B.ByteString


padLeft::Int->Char->B.ByteString->B.ByteString
padLeft targlen pad str 
                        | targlen<= len = str
                        | otherwise = B.append (B.replicate (targlen-len) pad) str
      where len = B.length str
padRight::Int->Char->B.ByteString->B.ByteString
padRight targlen pad str 
                        | targlen<= len = str
                        | otherwise = B.append str (B.replicate (targlen-len) pad)
      where len = B.length str

writeheader treeLen tailsLen rulesLen hushLen = runPut  serializeHeader
  where 
  serializeHeader::Put
  serializeHeader =
   do
     putByteString $  padLeft 8 '\0' $ pack "CTCDict"
     putByteString $  padLeft 8 '\0' $ pack "Intel" 
     putByteString $  padLeft 8 '\0' $ pack "BEL"
     putByteString $  padLeft 8 '\0' $ pack "03.03"
     putByteString $  padLeft 8 '\0' $ pack $ printf "%07d" treeLen
     putByteString $  padLeft 8 '\0' $ pack $ printf "%07d" tailsLen
     putByteString $  padLeft 8 '\0' $ pack $ printf "%07d" rulesLen
     putByteString $  padLeft 8 '\0' $ pack $ printf "%07d" hushLen
     putByteString $  padLeft 8 '\0' $ pack $ printf "%07d" 33
     putByteString $  padLeft 64 '\0' $ pack "xxx"
     putByteString $  padLeft 64 '\0' $ pack "xxx"

main = L.putStr $ writeheader 8 9 10 11 

