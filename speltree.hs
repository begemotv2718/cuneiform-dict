--import List
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC
import Data.Word
import Maybe
import Numeric
import Control.Monad
import Data.Tree
import qualified Data.Foldable as DF 
import Data.Bits

mapfromrus = Map.fromList $ zip [0::Int .. 31::Int] ['а'..'я'] 
rusletter lt = Map.findWithDefault '-' lt mapfromrus

type Letter = Int
data DictData = DictData { letter:: Letter, account::Float, terminal::Bool, lemma::[Int]} deriving (Eq)
instance Show DictData where
   show (DictData lt acc term lemma) =  [rusletter lt]  ++ show lemma 
type DictNode = Tree DictData
{--Properties: Nodes are arranged alphabetically with last letters first  --}
type DictTree = Forest DictData 

isEmpty::[Letter] -> Bool
isEmpty = null

data DictWord = DictWord { st::[Letter], wlemma::Int, freq::Float, freeTerm::Bool}
instance Show DictWord where
 show (DictWord st wl fr freeterm) = show (map rusletter st) ++ "lemma: "++show wl++" frequency: "++show fr++" terminal: "++show freeterm

fstletter::DictWord->Letter
fstletter = head. st 

fstlettereq::DictWord->DictWord->Bool
fstlettereq a b = fstletter a == fstletter b

removefstletter::[DictWord] -> [DictWord]
removefstletter = filter (not . isEmpty . st) . map removefstletterword

removefstletterword::DictWord->DictWord
removefstletterword a =DictWord { st = tail $ st a, wlemma=wlemma a, freq = freq a, freeTerm = freeTerm a}

sumdata::[DictWord]->Float
sumdata = sum . map freq 

issinglelet::DictWord->Bool
issinglelet  = isEmpty . tail. st 

findterminal::[DictWord]->Bool
findterminal lst = or $ map freeTerm lst  

getlemmas::[DictWord]->[Int]
getlemmas = map wlemma . filter (not.freeTerm) . filter issinglelet

unfolder::[DictWord]->(DictData,[[DictWord]]) 
unfolder b = (DictData { letter = fstletter $ head b, account = sumdata b, terminal = findterminal b, lemma = getlemmas b },
              groupBy fstlettereq $ removefstletter b) 



makeTree:: [DictWord]->DictTree
makeTree list = unfoldForest  unfolder (groupBy fstlettereq list)

-- Tree to binary conversion

mapAccumTree::(a->[b]->(b,c))->Tree a -> (b, Tree c)
mapAccumTree f (Node a []) = (b', Node c' [])
           where (b',c') = f a []
mapAccumTree f (Node a sf) = (b', Node c' cs)
           where (b',c') = f a bs
                 (bs,cs) = unzip $ map (mapAccumTree f) sf

mapAccumTree2::(a->[b]->b)->Tree a->Tree b
mapAccumTree2 f = snd . mapAccumTree (dupl . f) where dupl f x = (f x,f x)

fmapForest::([a]->[b])->Forest a->Forest b
fmapForest f [] = []
fmapForest f forest = zipWith Node (f rootlabels) (map (fmapForest f) subforests)
           where rootlabels = map rootLabel forest
                 subforests = map subForest forest


data IntermediateVertex = IVertex { dictdata::DictData, isLast::Bool, len::Int, subtreelen::Int}

calcLength::Forest IntermediateVertex->Forest IntermediateVertex
calcLength t = map (mapAccumTree2 mycalclength) t

mycalclength::IntermediateVertex->[IntermediateVertex]->IntermediateVertex
mycalclength dictv sublist = IVertex { dictdata=dictdata dictv, 
                                       isLast=isLast dictv,
                                       len = 1+lemmalen+ accntlen+addrlen subtrlen (isLast dictv),
                                       subtreelen = subtrlen}
                             where lemmalen = 2*(length. lemma.dictdata) dictv
                                   accntlen 
                                            | (account.dictdata) dictv >0.0 || (terminal.dictdata) dictv = 1
                                            | otherwise = 0
                                   subtrlen = (sum $ map len sublist) + (sum $ map subtreelen sublist) 
                                   addrlen s lst 
                                             | s<0 = -1
                                             | ((s>=0) && (s<32)) || lst  = 1
                                             | (s>=32) && (s<32*128) && not lst =2
                                             | s>= 32*128 && not lst = 3

markLast::DictTree-> Forest IntermediateVertex
markLast = fmapForest marklastv where 
                                 marklastv [] = []
                                 marklastv a = map notlast (init a) ++ [setlast $ last a]
                                 setlast::DictData->IntermediateVertex
                                 setlast a = IVertex a True 0 0
                                 notlast a = IVertex a False 0 0  
                                       
--serializeDictNode::DictNode->B.ByteString

setBit2 = flip setBit

serializeIV::IntermediateVertex->B.ByteString
serializeIV (IVertex a lst ln sbtrln) = vertV a sbtrln `B.append` postfixlemmas a `B.append` postfixaccount a `B.append` postfixaddr lst sbtrln  
postfixlemmas::DictData->B.ByteString
postfixlemmas (DictData _ _ _ []) = B.empty
postfixlemmas (DictData _ account terminal lst) = (initpsfx $ init lst) `B.append` (lastpsfx account terminal $ last lst)
          where initpsfx lst = B.concat $ map makelemma lst
                makelemma x = B.singleton (settail.setcont $ enter0 x) `B.append` (B.singleton $ enter1 x)
                enter0::Int->Word8
                enter0 x = shiftL (fromIntegral x .&. 31::Word8) 3
                enter1::Int->Word8
                enter1 x = fromIntegral (shiftR x 5)      
                lastpsfx acc term x = (B.singleton.settail.setcont $ enter0 x) `B.append` (B.singleton $ enter1 x)
                -- we may need to adjust in case of account and terminal absent
settail::Word8->Word8
settail = setBit2 1
setcont = setBit2 0
setaccount = setBit2 2
setBit2If::Bool->Int->Word8->Word8
setBit2If cond bit = if cond then setBit2 bit else id

postfixaccount (DictData _ acc term _) = if acc == 0.0 then B.empty else B.singleton $ settail. setaccount. setBit2If term 3 $ makefreq acc
              where makefreq acc = shiftL (round acc .&. 7::Word8) 5 

postfixaddr::Bool->Int->B.ByteString
postfixaddr lst subtreelen = if lst then makelast else encodetreelen subtreelen
        where
           makelast = B.singleton 0
           encodetreelen::Int->B.ByteString
           encodetreelen x 
                           | x<32 = B.singleton (shift0 x)
                           | (x>=32) && (x<32*128) = B.singleton (setBit2 2 $ shift01 x) `B.append` (B.singleton $ shift1 x)
                           | x>=32*128 = B.singleton (setBit2 2 $ shift02 x) `B.append` B.singleton (setBit2 0 $ shift12 x) `B.append` (B.singleton $ shift2 x)
              where
              --not correct here!!!
              shift0::Int->Word8
              shift0 x = shiftL (fromIntegral x .&. 31::Word8) 3
              shift01 x = shift0 $ shiftR x 7
              shift02 x = shift0 $ shiftR x 15 
              shift1::Int->Word8
              shift1 x = shiftL (fromIntegral x .&. 127::Word8) 1
              shift12 x = shift1 $ shiftR x 8
              shift2::Int->Word8
              shift2 x = fromIntegral x .&. 255::Word8  

vertV (DictData lt _ _ _) ln = (B.singleton.setcont.setnoterm ln.setkey) lt
      where 
           setkey lt = shiftL (fromIntegral lt .&. 63::Word8) 2 
           setnoterm ln = setBit2If (ln > 0) 1 


unfoldForest2List:: Int->Forest IntermediateVertex -> [Maybe (Tree IntermediateVertex)]
unfoldForest2List alphsize forest = [find (((==) i).letter.dictdata.rootLabel) forest | i<-[0 .. alphsize-1]] 


extractPrepend:: Int->Maybe (Tree IntermediateVertex)->[Maybe (Tree IntermediateVertex)]
extractPrepend alphsize (Just vert) = setIfTerminal vert:(unfoldForest2List alphsize $ subForest vert) 
                                  where 
                                   setIfTerminal vert = if isNotTerminal vert then Nothing else Just $ clearSubnodes vert
                                   isNotTerminal vert = (isEmpty.lemma.dictdata.rootLabel) vert && (not.terminal.dictdata.rootLabel) vert 
                                   clearSubnodes a = Node { rootLabel = rootLabel a, subForest = []}
extractPrepend alphsize Nothing = replicate (alphsize+1) Nothing
    
vertplen::Int
vertplen = 3 -- Length of the p-type vertex                                     

unfoldLevel2::Int->Forest IntermediateVertex->[Maybe (Tree IntermediateVertex)]
unfoldLevel2  alphsize = concatMap (extractPrepend alphsize) . unfoldForest2List alphsize

serializeMaybeTree::Int->Maybe (Tree IntermediateVertex)->(Int, B.ByteString, B.ByteString)
serializeMaybeTree offset Nothing = (offset-vertplen, B.replicate (fromIntegral vertplen) (0::Word8),B.empty)
serializeMaybeTree offset (Just tr) = (offset + (len  rl-1)+subtreelen rl  -vertplen,
                                     vertexP offset rl, 
                                     B.tail $ DF.foldMap serializeIV tr) 
                  where rl = rootLabel tr
                        vertexP offset (IVertex a lst ln sbtrln) = 
                                           (B.singleton $ setexists $ setcont $ setnoterm sbtrln $ vertpShift0 offset) 
                                          `B.append` (B.singleton $ vertpShift1 offset) 
                                          `B.append` (B.singleton $ vertpShift2 offset)
                        setcont  = setBit2 0
                        setnoterm ln = setBit2If (ln > 0) 1 
                        setexists = setBit2 3
                        vertpShift0 x = shiftL (fromIntegral (shiftR x 16).&. (31::Word8)) 3 
                        vertpShift1 x = fromIntegral (shiftR x 8) .&. 255::Word8  
                        vertpShift2 x = fromIntegral x .&. 255::Word8

foldSerializeMaybeTree::Int->(Int,B.ByteString,B.ByteString)->Maybe (Tree IntermediateVertex)->(Int,B.ByteString,B.ByteString)
foldSerializeMaybeTree alphsize (shift,prefixes,body) treevert = (resshift, prefixes `B.append` resprefixes,body `B.append` resbody)
                                            where (resshift, resprefixes, resbody) = serializeMaybeTree shift treevert

--Main function of this section::
serializeDictTree::Int->DictTree->B.ByteString
serializeDictTree alphsize tree = prefixes `B.append` body
     where
     (len,prefixes,body) = foldl' (foldSerializeMaybeTree alphsize) (vertplen*(arraylen-1),B.empty,B.empty) convtree
     arraylen = alphsize*(alphsize+1)
     convtree = unfoldLevel2 alphsize $ (calcLength.markLast) tree  

                                    
-- Input/output
{--
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
                      return  DictWord { st = string, freq = freq, wlemma = lemma, freeTerm = False}
parsedata (a:b:[]) = do
                      freq <- parseFloat b
                      string <-letters russianAlphabet a
                      return DictWord {st = string, freq =freq, wlemma = -1, freeTerm = True}
parsedata _ = Nothing


maybefst:: Maybe (a,b) -> Maybe a
maybefst (Just x) = Just $ fst x
maybefst _ = Nothing

parseInt::BC.ByteString -> Maybe Int
parseInt = maybefst . BC.readInt 
parseFloat::BC.ByteString -> Maybe Float
parseFloat = maybefst . listToMaybe . readFloat . BC.unpack

parsefile::BC.ByteString->Maybe [DictWord]
parsefile = (mapM parsedata) . (map BC.words) . BC.lines 

getDictTree::BC.ByteString->DictTree
getDictTree = fromMaybe [] . (liftM makeTree). parsefile
--}


type Alphabet = Map Char Int
russianBig = ['А' .. 'Я' ] 
russianSmall = ['а' .. 'я']
russianAlphabet::Alphabet
russianAlphabet = Map.fromList $ (zip russianBig [0::Int .. 31::Int]) ++ (zip russianSmall [0::Int .. 31::Int])

letters:: Alphabet->String -> Maybe [Letter]
letters abc s = mapM (flip Map.lookup abc) s  

--Convert triple word lemma freq into data
parsedata::[String]->Maybe DictWord
parsedata (a:b:c:[]) = do
                      lemma <- parseInt b
                      freq <-parseFloat c
                      string <- letters russianAlphabet a
                      return  DictWord { st = string, freq = freq, wlemma = lemma, freeTerm = False}
parsedata (a:b:[]) = do
                      freq <- parseFloat b
                      string <-letters russianAlphabet a
                      return DictWord {st = string, freq =freq, wlemma = -1, freeTerm = True}
parsedata _ = Nothing


maybefst:: Maybe (a,b) -> Maybe a
maybefst (Just x) = Just $ fst x
maybefst _ = Nothing

parseInt::String -> Maybe Int
parseInt = maybefst . listToMaybe. readDec 
parseFloat::String -> Maybe Float
parseFloat = maybefst . listToMaybe . readFloat 

parsefile::String->Maybe [DictWord]
parsefile = (mapM parsedata) . (map words) . lines 

getDictTree::String->DictTree
getDictTree = fromMaybe [] . (liftM makeTree). parsefile
main = do
-- content <-BC.getContents
-- print $ ((liftM makeTree) . parsefile) content
 tree<-(liftM getDictTree) $ getContents
 putStrLn $ drawForest $ map (fmap show) tree
 B.writeFile "./dictree.dat" $ serializeDictTree 32 tree
