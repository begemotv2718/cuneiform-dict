import List
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC
import Data.Word
import Maybe
import Numeric
import Control.Monad

type Letter = Int
data DictNode = DictNode { letter:: Letter, account::Float, terminal::Bool, lemma::[Int],  subnodes::[DictNode] } deriving (Eq,Show)
{--Properties: Nodes are arranged alphabetically with last letters first  --}
type DictTree = [DictNode]

isEmpty::[Letter] -> Bool
isEmpty = null

updateTree:: [Letter] -> Int->Float -> DictTree -> DictTree
updateTree [] lemmanum freq tree = tree
updateTree (lt:tail) lemmanum freq [] = [
                         DictNode { 
                               letter=lt,
                               account = freq,
                               terminal = isEmpty tail,
                               lemma = if isEmpty tail then [lemmanum] else [], 
                               subnodes = updateTree tail lemmanum freq []} ]
updateTree (lt:tail) lemmanum freq (node:nodes) =
-- Three cases lt == letter node, then update node
   if lt == letter node
       then (updateNode node lemmanum freq tail):nodes
       else if lt < letter node
            then node:(updateTree (lt:tail) lemmanum freq nodes)
            else (newNode (lt:tail) lemmanum freq):node:nodes

newNode:: [Letter]->Int->Float->DictNode
newNode (lt:tail) lemmanum freq =  DictNode{
                     letter = lt
                    ,account = freq
                    ,terminal = isEmpty tail
                    ,lemma = if isEmpty tail then [lemmanum] else []
                    ,subnodes = updateTree tail lemmanum freq []
                                  }

updateNode:: DictNode->Int->Float->[Letter]->DictNode
updateNode nd lemmanum freq wordrest  = DictNode{ letter = letter nd 
                                        ,account = freq + account nd
                                        ,terminal = isEmpty wordrest || terminal nd
                                        ,lemma = if isEmpty wordrest then lemmanum:(lemma nd) else (lemma nd)
                                        ,subnodes = updateTree wordrest lemmanum freq $ subnodes nd
                                       }


data DictWord = DictWord { st::[Letter], wlemma::Int, freq::Float}

updateTreeWord::DictTree->DictWord->DictTree
updateTreeWord t w = updateTree (st w) (wlemma w) (freq w) t  


makeTree:: [DictWord]->DictTree
makeTree list = foldl updateTreeWord [] list

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
