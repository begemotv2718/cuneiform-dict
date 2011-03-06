import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC
import Data.Word 
main = do
 B.putStrLn $ B.pack [128::Word8 .. 159::Word8]
 B.putStrLn $ B.pack $ [160::Word8 .. 175::Word8] ++ [224::Word8 .. 239::Word8]
 putStrLn $ BC.unpack $ B.pack [32::Word8 .. 60::Word8]


