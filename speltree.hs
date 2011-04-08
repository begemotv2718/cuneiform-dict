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

unfoldLevel2::Int->Forest DictData->[Maybe (Tree DictData)]
unfoldLevel2  alphsize = concatMap (extractPrepend alphsize) . unfoldForest2List alphsize

unfoldForest2List:: Int->Forest DictData -> [Maybe (Tree DictData)]
unfoldForest2List alphsize forest = [find (((==) i).letter.rootLabel) forest | i<-[0 .. alphsize-1]] 


extractPrepend:: Int->Maybe (Tree DictData)->[Maybe (Tree DictData)]
extractPrepend alphsize (Just vert) = setIfTerminal vert:(unfoldForest2List alphsize $ subForest vert) 
                                  where 
                                   setIfTerminal vert = if isNotTerminal vert then Nothing else Just $ clearSubnodes vert
                                   isNotTerminal vert = (isEmpty.lemma.rootLabel) vert && (not.terminal.rootLabel) vert 
                                   clearSubnodes a = Node { rootLabel = rootLabel a, subForest = []}
extractPrepend alphsize Nothing = replicate (alphsize+1) Nothing
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

setBit2 = flip setBit
setBit2If::Bool->Int->Word8->Word8
setBit2If cond bit = if cond then setBit2 bit else id

data IntermediateVertex = IVertex {vertexV::Word8, postfixEnding::B.ByteString, postfixAccount::B.ByteString, postfixAddr::B.ByteString, subTree::B.ByteString }
instance Show IntermediateVertex where
  show iv = showHex (fromIntegral $ vertexV iv) ""

showIvData::(Show a)=>Int->[Maybe (Tree a)]->String
showIvData alphsize dat = concatMap showmtuple $ numberList 0 dat
     where 
     showmtuple (num, tree) = 
        if isJust tree then
          labelfstltrs num ++ "\n" ++ drawTree ( fmap show $ fromJust tree)++"\n"
        else ""
     labelfstltrs num = [rusletter fstletter] ++ if sndletter>0 then [rusletter sndletter] else ""
        where 
           fstletter = num `div` (alphsize+1)
           sndletter = (num `mod` (alphsize+1)) -1
                                 

numberList::Int->[a]->[(Int,a)]
numberList startindex lst = snd $ mapAccumL f startindex lst
            where 
                  f::Int->a->(Int,(Int,a))
                  f acc el = (acc+1,(acc,el))

convDictStage1::[Maybe (Tree DictData)]->[Maybe (Tree IntermediateVertex)] 
convDictStage1 = map (fmap $ fmap convVertex) 

convDictStage2::[Maybe (Tree IntermediateVertex)]->[Maybe (Tree IntermediateVertex)] 
convDictStage2 = map (fmap  convertorStage2 ) 


convertorStage2::Tree IntermediateVertex -> Tree IntermediateVertex
convertorStage2 = mapAccumTree2 convertOperation

convertOperation::IntermediateVertex->[IntermediateVertex]->IntermediateVertex
convertOperation vertex subvertexes = IVertex { 
                 vertexV = vertexV vertex,
                 postfixEnding = postfixEnding vertex,
                 postfixAccount = postfixAccount vertex,
                 postfixAddr = if postfixAddr vertex == B.singleton 0 
                                then B.singleton 0 
                                else encodePostfixAddr $ fromIntegral $ B.length resSubTree,
                 subTree = resSubTree
                 }
                where 
                   resSubTree = summarize subvertexes
                   summarize::[IntermediateVertex]->B.ByteString
                   summarize = B.concat . map joinelements
                   joinelements::IntermediateVertex->B.ByteString
                   joinelements (IVertex a b c d e) = B.concat [B.singleton a,b,c,d,e]
                   --Add vertex operations according to taste

convVertex::DictData->IntermediateVertex
convVertex a = IVertex { vertexV = encodeVertexV $ letter a, 
                         postfixEnding = encodePostfixEnding $ lemma a,
                         postfixAccount = encodePostfixAccount a,
                         postfixAddr = B.empty,
                         subTree = B.empty
                       } 

encodeVertexV::Letter->Word8
encodeVertexV lt = shiftL (fromIntegral lt .&. 63::Word8) 2 

encodePostfixEnding::[Int]->B.ByteString
encodePostfixEnding lst = B.concat $ map makelemma lst
          where 
                makelemma x = B.singleton (settail.setcont $ enter0 x) `B.append` (B.singleton $ enter1 x)
                enter0::Int->Word8
                enter0 x = shiftL (fromIntegral (shiftR x 5) .&. 31::Word8) 3
                enter1::Int->Word8
                enter1 x = fromIntegral x .&. 255::Word8      

settail::Word8->Word8
settail = setBit2 1
setcont = setBit2 0
setaccount = setBit2 2

encodePostfixAccount (DictData _ acc term _) = if acc == 0.0 then B.empty else B.singleton $ settail. setaccount. setBit2If term 3 $ makefreq acc
              where makefreq acc = shiftL (round acc .&. 7::Word8) 5 

encodePostfixAddr::Int->B.ByteString
encodePostfixAddr x 
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

vertplen::Int
vertplen = 3 -- Length of the p-type vertex                                     

serializeMaybeTree::Int->Maybe (Tree IntermediateVertex)->(Int, B.ByteString, B.ByteString)
serializeMaybeTree offset Nothing = (offset-vertplen, B.replicate (fromIntegral vertplen) (0::Word8),B.empty)
serializeMaybeTree offset (Just tr) = (offset + (fromIntegral $ B.length $ subtr tr) -vertplen, --(len  rl-1)+subtreelen rl  -vertplen,
                                     vertexP offset (fromIntegral $ B.length $ subtr tr), 
                                     subtr tr) 
                  where rl = rootLabel tr
                        vertexP offset ln = 
                                           (B.singleton $ setexists $ setcont $ setnoterm ln $ vertpShift0 offset) 
                                          `B.append` (B.singleton $ vertpShift1 offset) 
                                          `B.append` (B.singleton $ vertpShift2 offset)
                        setcont  = setBit2 0
                        setnoterm ln = setBit2If (ln > 0) 1 
                        setexists = setBit2 2
                        vertpShift0 x = shiftL (fromIntegral (shiftR x 16).&. (31::Word8)) 3 
                        vertpShift1 x = fromIntegral (shiftR x 8) .&. 255::Word8  
                        vertpShift2 x = fromIntegral x .&. 255::Word8
                        subtr::Tree IntermediateVertex->B.ByteString
                        subtr tr = subtr' $ rootLabel tr
                        subtr' l = B.concat [postfixEnding l, postfixAccount l, postfixAddr l, subTree l]

foldSerializeMaybeTree::Int->(Int,B.ByteString,B.ByteString)->Maybe (Tree IntermediateVertex)->(Int,B.ByteString,B.ByteString)
foldSerializeMaybeTree alphsize (shift,prefixes,body) treevert = (resshift, prefixes `B.append` resprefixes,body `B.append` resbody)
                                            where (resshift, resprefixes, resbody) = serializeMaybeTree shift treevert


type TestTuples = [TestTuple]
data TestTuple = TT { pref::B.ByteString, suf::B.ByteString}
instance Show TestTuple
  where 
    show (TT a b) = "("++(concat $ map showhex $ B.unpack a) ++","++(concat $ map showhex $ B.unpack b)++")"
      where
        showhex::Word8->String
        showhex a = hexdigit (shiftR a 4) ++ hexdigit (a .&. 0x0f)
        hexdigit::Word8->String
        hexdigit a 
                  | a<10 = show a
                  | (a==10) = "a"
                  | (a==11) = "b"
                  | (a==12) = "c"
                  | (a==13) = "d"
                  | (a==14) = "e"
                  | (a==15) = "f"            
            
foldSerializeMaybeTreeTest::Int->(Int,TestTuples)->Maybe (Tree IntermediateVertex)->(Int,TestTuples)
foldSerializeMaybeTreeTest alphsize (shift,lst) treevert = (resshift, lst++[TT resprefixes resbody ])
                                            where (resshift, resprefixes, resbody) = serializeMaybeTree shift treevert

--Main function of this section::
serializeDictTree::Int->DictTree->B.ByteString
serializeDictTree alphsize tree = maxlevel `B.append` prefixes `B.append` body
     where
     (len,prefixes,body) = foldl' (foldSerializeMaybeTree alphsize) (vertplen*arraylen,B.empty,B.empty) convtree
     arraylen = alphsize*(alphsize+1)
     convtree = convDictStage1 $ unfoldLevel2 alphsize tree  
     maxlevel = B.singleton $ shiftL (2::Word8) 2 
serializeDictTreeTest::Int->DictTree->TestTuples
serializeDictTreeTest alphsize tree = snd $ foldl' (foldSerializeMaybeTreeTest alphsize) (vertplen*arraylen,[]) convtree 
     where
     arraylen = alphsize*(alphsize+1)
     convtree = convDictStage1 $ unfoldLevel2 alphsize tree  
     maxlevel = B.singleton $ shiftL (2::Word8) 2 
                                    


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
 --B.writeFile "./dictree.dat" $ serializeDictTree 32 tree
 putStrLn $ showIvData 32 $ unfoldLevel2 32 tree 
