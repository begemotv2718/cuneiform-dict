module WriteHeader

where
import Text.Printf
import Data.ByteString.Char8 as B
import Data.Word
import Data.Binary.Put
import Data.ByteString.Lazy as L hiding (pack)



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

writeheader::Int->Int->Int->Int->L.ByteString
writeheader treeLen tailsLen rulesLen hushLen = runPut  serializeHeader
  where 
  serializeHeader::Put
  serializeHeader =
   do
     putByteString $  padRight 8 '\0' $ pack "CTCDict"
     putByteString $  padRight 8 '\0' $ pack "Intel" 
     putByteString $  padRight 8 '\0' $ pack "BEL"
     putByteString $  padRight 8 '\0' $ pack "03.03"
     putByteString $  padRight 8 '\0' $ pack $ printf "%07d" treeLen
     putByteString $  padRight 8 '\0' $ pack $ printf "%07d" tailsLen
     putByteString $  padRight 8 '\0' $ pack $ printf "%07d" rulesLen
     putByteString $  padRight 8 '\0' $ pack $ printf "%07d" hushLen
     putByteString $  padRight 8 '\0' $ pack $ printf "%07d" (33::Int)
     putByteString $  padRight 64 '\0' $ pack (['\x80'..'\x85']++['\xf0','\x86','\x87','\xf6']++['\x89'..'\x93']++['\xf9']++['\x94'..'\x98']++['\x9b'..'\x9f']++['\x27','\0'])
     putByteString $  padRight 64 '\0' $ pack (['\xa0'..'\xa5']++['\xf1','\xa6','\xa7','\xf7']++['\xa9'..'\xaf']++['\xe0'..'\xe3']++['\xf8']++['\xe4'..'\xe8']++['\xeb'..'\xef']++['\x27','\0'])

