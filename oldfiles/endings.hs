import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Word
import Data.Bits
import Data.Set (Set, size)
import qualified Data.Set as Set
import Data.List
import Data.Array.IArray
import Data.Ord
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import CommonType
import System.Environment



data Endian = Little | Big

lowestByte::(Integral a, Bits a) => a->(Word8,a)
lowestByte x= ((fromIntegral x) .&. (255::Word8), shiftR x 8)

encodeWord16::Endian->Word16->ByteString
encodeWord16 Little a = fst $ B.unfoldrN 2  (Just . lowestByte) a  
encodeWord16 Big a = B.reverse $ encodeWord16 Little a

encodeWord32::Endian->Word32->ByteString
encodeWord32 Little a = fst $ B.unfoldrN 4  (Just . lowestByte) a  
encodeWord32 Big a = B.reverse $ encodeWord32 Little a

data VarTable = VarTable { tablenum::Int, maxlen::Int, mask::Word32}

encodeVarTable::Endian->VarTable->ByteString
encodeVarTable e t = B.concat [encodeWord16 e $ fromIntegral $ tablenum t, encodeWord16 e $ fromIntegral $ maxlen t, encodeWord32 e $ mask t]  

subsetToBitMap::(Ord a,Eq a) => Set a->Set a->Maybe Word32
subsetToBitMap sbs set = if Set.isSubsetOf sbs set && Set.size set <= 32 
    then Just (setBits (0::Word32) $ ordListIndexes (Set.toAscList sbs) (Set.toAscList set)) 
    else Nothing

--given two ordered lists (first is fully contained in the second),
--give positions of the elements of the first in the second
--indexes are returned in the reverse order
ordListIndexes::(Ord a, Eq a)=>[a]->[a]->[Int]
ordListIndexes sublist list = snd3 (foldl' iterateindexes (0,[],sublist) list)
  where iterateindexes::(Ord a, Eq a)=> (Int,[Int],[a])->a->(Int,[Int],[a])
        iterateindexes (curpos,indexes,[]) elem = (curpos,indexes,[])
        iterateindexes (curpos,indexes,x:xs) elem = if (x == elem) then (curpos+1,curpos:indexes,xs) else (curpos+1, indexes, x:xs)
        snd3 (a,b,c) = b

setBits::(Integral a, Bits a) =>a->[Int]->a
setBits = foldl' setBit 

mkEndingPackTable::AllSetS->[[Int]]->(ByteString,ByteString)
mkEndingPackTable sts packed =  fst $ mapAccumL  localpackendings (B.empty, B.empty) packed 
  where localpackendings::(ByteString,ByteString)->[Int]->((ByteString,ByteString), Int)
        localpackendings (tableoffsets, tableends) setlst = 
              ((tableoffsets `B.append` (encodeWord32 Little $ fromIntegral $ B.length tableends), 
               tableends `B.append` packEndingListInt sts setlst ), B.length tableends)

packEndingListInt::AllSetS->[Int]->ByteString
packEndingListInt sts = packEndingSetS . Set.unions . map ((!) sts)

packEndingSetS::SetS->ByteString
packEndingSetS = (B.concat . map packEnding) . Set.toAscList

packEnding::Letters->ByteString
packEnding = markend. B.pack. map fromIntegral
  where markend::ByteString->ByteString
        markend str = (B.init str) `B.append` B.singleton ( (B.last str) .|. (0x80::Word8) )

packSetSbyInd::AllSetS->[Int]->ByteString
packSetSbyInd sts = packEndingSetS . mkSetSfromInd sts

mkSetSfromInd::AllSetS->[Int]->SetS
mkSetSfromInd sts = Set.unions. map ((!) sts)

mkVarTable::AllSetS->[[Int]]->[VarTable]
mkVarTable sts packed = map mksinglevartable $ assocs sts
   where 
      mksinglevartable::(Int,SetS)->VarTable
      mksinglevartable (num,set) = VarTable {tablenum = (revassoc ! num), maxlen = maxlen set, mask = fromJust $ subsetToBitMap (sts ! num) (tblsts ! (revassoc ! num)) }
      revassoc::Array Int Int
      --revassoc = array (bounds sts)  $ (reverseAssoc::[(Int,[Int])]->[(Int,Int)]) $ (numberList::Int->[[Int]]->[(Int,[Int])]) 0 packed 
      revassoc = array (bounds sts)  $ reverseAssoc $ numberList 0 packed 
      tblsts = listArray0 0 $ map (mkSetSfromInd sts) packed
      maxlen::SetS->Int
      maxlen set = Set.findMax $ Set.map length set 

mkVarTableBS::AllSetS->[[Int]]->ByteString
mkVarTableBS sts packed = B.concat $ map (encodeVarTable Little) $ mkVarTable sts packed 

----------------------------------------------------------




parseFile:: String->AllSetS
parseFile = listArray0 0 . map line2Set .  lines
   where line2Set = Set.fromList .map letters' .words   

parseFile0 = map line2Set .  lines
   where line2Set = Set.fromList .map letters' .words
-------------------------------------------------------------

findMaxSet::AllSetS->[Int]->Int
findMaxSet sts = maximumBy $ comparing (size.(!) sts) 

findMinComposition::AllSetS->Int->[Int]->Int
findMinComposition sts ind = minimumBy $ comparing (size.makeunion)
    where makeunion ind' = Set.union (sts ! ind) (sts ! ind')

findMinComposition'::AllSetS->SetS->[Int]->Int
findMinComposition' sts set = minimumBy $ comparing (size.makeunion')
    where makeunion' ind = Set.union set (sts ! ind)

packOneBin:: AllSetS->Int->[Int]->([Int],[Int])
packOneBin sts oneset []  = ([],[])
packOneBin sts oneset lst = (binincluded, binexcluded)
        where binincluded = lst \\ binexcluded
              binexcluded = snd $ iterateUntil f iteratestop (sts ! oneset, lst)
                  where 
                      iteratestop (set, lst) = size set > 32 || null lst
                      f::(SetS,[Int])->(SetS,[Int])
                      f (set,idxs) = ( Set.union set (sts ! mcompind), idxs\\[mcompind])
                         where mcompind = findMinComposition' sts set idxs 

packAllBins::AllSetS->[[Int]]
packAllBins sts = fst $ iterateWhile oneiteration iteratestop ([],indices sts)
        where iteratestop (lstlst, left) = null left
              oneiteration (lstlst, left) = ((maxset:onebin):lstlst, left')
                   where (onebin, left') =  (packOneBin sts maxset (left\\[maxset]))
                         maxset = findMaxSet sts left
                        

iterateUntil::(a->a)->(a->Bool)->a->a
iterateUntil f stopcond start = if stopcond (f start) 
                                 then start 
                                 else iterateUntil f stopcond (f start)

iterateWhile::(a->a)->(a->Bool)->a->a
iterateWhile f stopcond start = if stopcond start 
                                 then start 
                                 else iterateWhile f stopcond (f start)
----------------------------------------------------------------------------------------
reverseAssoc::[(a,[b])]->[(b,a)]
reverseAssoc = concatMap f
 where f (a,lst) = map (\s->(s,a)) lst

numberList::Int->[a]->[(Int,a)]
numberList startindex lst = snd $ mapAccumL f startindex lst
            where 
                  f::Int->a->(Int,(Int,a))
                  f acc el = (acc+1,(acc,el))

listArray0:: Int->[a]->(Array Int a)
listArray0 startindex list = array (startindex, endindex-1) endlist
            where (endindex, endlist) = mapAccumL f startindex list 
                  f::Int->a->(Int,(Int,a))
                  f acc el = (acc+1,(acc,el))

main = do
     dat<-getContents
     args<-getArgs
     --let allsets = parseFile dat in
      --print $ packOneBin allsets 6 (indices allsets\\[6])
     --print $ packAllBins $ parseFile dat
     let sts = parseFile dat 
         packed = packAllBins sts
         vartable =  mkVarTableBS sts packed
         endaddrtable =  fst $ mkEndingPackTable sts packed
         pkdendings =  snd $ mkEndingPackTable sts packed
      in
       (putStrLn $ show (B.length pkdendings)++" "++show (B.length vartable) ++" " ++ show (B.length endaddrtable)) >>
       print packed >>
       B.writeFile (head args) (pkdendings `B.append` vartable `B.append` endaddrtable)
