import List
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC
import Data.Word
import Maybe
import Numeric
import Control.Monad
import Data.Tree

type Letter = Int
data DictData = DictData { letter:: Letter, account::Float, terminal::Bool, lemma::[Int]} deriving (Show,Eq)
type DictNode = Tree DictData
{--Properties: Nodes are arranged alphabetically with last letters first  --}
type DictTree = Forest DictData 

isEmpty::[Letter] -> Bool
isEmpty = null

data DictWord = DictWord { st::[Letter], wlemma::Int, freq::Float}

fstletter::DictWord->Letter
fstletter = head. st 

fstlettereq::DictWord->DictWord->Bool
fstlettereq a b = fstletter a == fstletter b

removefstletter::[DictWord] -> [DictWord]
removefstletter = filter (not . isEmpty . st) . map removefstletterword

removefstletterword::DictWord->DictWord
removefstletterword a =DictWord { st = tail $ st a, wlemma=wlemma a, freq = freq a}

sumdata::[DictWord]->Float
sumdata = sum . map freq 

issinglelet::DictWord->Bool
issinglelet  = isEmpty . tail. st 

findterminal::[DictWord]->Bool
findterminal  = isJust. find issinglelet  

getlemmas::[DictWord]->[Int]
getlemmas = map wlemma . filter issinglelet

unfolder::[DictWord]->(DictData,[[DictWord]]) 
unfolder b = (DictData { letter = fstletter $ head b, account = sumdata b, terminal = findterminal b, lemma = getlemmas b },
              groupBy fstlettereq $ removefstletter b) 



makeTree:: [DictWord]->DictTree
makeTree list = unfoldForest  unfolder (groupBy fstlettereq list)

-- Input/output

type Alphabet = Map Word8 Int
russianBigCp866 = [ 128::Word8 .. 159::Word8] 
russianSmallCp866 = [160::Word8 ..175::Word8 ] ++ [ 224::Word8 .. 239::Word8]
russianAlphabet::Alphabet
russianAlphabet = Map.fromList $ (zip russianBigCp866 [0::Int .. 31::Int]) ++ (zip russianSmallCp866 [0::Int .. 31::Int])

letters:: Alphabet->BC.ByteString -> Maybe [Letter]
letters abc s = mapM (flip Map.lookup abc) (B.unpack s)  

--Convert triple word lemma freq into data
parsedata::[BC.ByteString]->Maybe DictWord
parsedata (a:b:c:[]) = do
                      lemma <- parseInt b
                      freq <-parseFloat c
                      string <- letters russianAlphabet a
                      return  DictWord { st = string, freq = freq, wlemma = lemma}
parsedata _ = Nothing


maybefst:: Maybe (a,b) -> Maybe a
maybefst (Just x) = Just $ fst x
maybefst _ = Nothing

parseInt::BC.ByteString -> Maybe Int
parseInt = maybefst . BC.readInt 
parseFloat::BC.ByteString -> Maybe Float
parseFloat = maybefst . listToMaybe . readFloat . BC.unpack


parsefile = (mapM parsedata) . (map BC.words) . BC.lines 

main = do
 content <-BC.getContents
 print $ ((liftM makeTree) . parsefile) content
