--What we have (baseform, [LetterIndex]) (dic file) + (LetterIndex, [AffRule]) (aff file). This is what hunspell provides
--We will likely have the following additional file (word, freq)
--Likely we will have the following intermediate form [(Stem, [Suffix])], and [(Stem,freq,[Suffix])]
--We will need to convert [(Stem,freq,[Suffix])] into [(Stem,freq,SuffixIndex)]+Array SuffixIndex SuffixSet
import Data.Array.IArray
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe
import Data.Monoid
import CommonType
import Data.Tuple
import Data.Char
import Text.Parsec.Char
import Text.Parsec.String
import Text.Parsec.Combinator
import Text.Parsec.Prim
import ParseAffFile
import Control.Monad
import System.Environment

type AffixList = M.Map LetterIdx [AffRule] -- mapping between letter index and affix rule. This is a result of parsing aff file
data DicFileRec = DicFileRec { baseform:: LetterWord, modifiers:: [LetterIdx] } -- record in a dic file.
instance Show DicFileRec where
    show (DicFileRec b m) = "DicFileRec { "++ map belletter b ++ show m ++ "}"

data WordNest = WordNest { stem:: [Letter], suffixes :: SuffixSet } 
instance Show WordNest where 
  show (WordNest stem suf) = "WordNest{ "++map belletter stem ++ "; "++ showsuffixset suf ++"}\n"
type SuffixSet = S.Set [Letter]
showsuffixset s = intercalate ", " $ map (map belletter) (S.toAscList s) 

--Data STreePlus a b = Node [ EdgePlus a b] | Leaf b
--Data EdgePlus a b = (Label a, StreePlus a b)

type SuffixMap = M.Map LetterWord [ReplRule] -- we wand some kind of  

mkSuffixMap:: AffRule->Maybe SuffixMap
mkSuffixMap (Sfx r m) = Just (M.fromList $ map (\x->(x,[r])) $ allcombinations m)    
mkSuffixMap _ = Nothing
mkSuffixMaps :: [AffRule]->SuffixMap
mkSuffixMaps = M.unionsWith (++) . mapMaybe mkSuffixMap   

type SfxList = M.Map LetterIdx SuffixMap
mkSfxList :: AffixList->SfxList
mkSfxList = M.map mkSuffixMaps 

joinAllSuffixMaps::SfxList->[LetterIdx]->SuffixMap
joinAllSuffixMaps slst idxs = M.unionsWith (++) $ mapMaybe (`M.lookup` slst ) idxs 

getRules::SuffixMap->LetterWord->[ReplRule]
getRules m w = concat . mapMaybe (`M.lookup` m) $ tails w
-- case dropWhile isNothing $ map (`M.lookup` m ) $ tails w 
--                      of [] -> []
--                         Just lst:lsts -> lst
--                         _ -> []

applysfxrule::LetterWord->ReplRule->Maybe LetterWord
applysfxrule w r = do
   stripped <- stripPrefix (reverse $ matchGroup r) $ reverse w
   return ( reverse stripped ++  replacementGroup r)

mkWordNest :: SfxList->DicFileRec->WordNest
mkWordNest slst rec = WordNest stm (S.fromList sfxs)
              where 
               (stm, sfxs) = commonPrefix forms 
               forms = uniq $ baseform rec:mapMaybe (applysfxrule (baseform rec)) rules
               rules = getRules sfxmap $ baseform rec 
               sfxmap = joinAllSuffixMaps slst $ modifiers rec 



data Annotation = Annotation { wordsuffixes::SuffixSet, wordfreq::Float}
instance Show Annotation where
   show (Annotation s f) = "Annotation { " ++ showsuffixset s ++ " | " ++ show f ++"}"
instance Monoid Annotation where
  mempty = Annotation  (S.empty:: S.Set [Letter]) 0.0  
  Annotation a1 b1 `mappend` Annotation a2 b2 = Annotation (mappend a1 a2) (b1+b2) 

type StemSuffixMap = M.Map LetterWord Annotation
buildStemSuffixMap:: [WordNest]->StemSuffixMap
buildStemSuffixMap wds = M.unionsWith mappend $ map (\x->M.singleton (stem x) (Annotation (suffixes x) 0.0)) wds 

matchStemSuffixMap::StemSuffixMap->LetterWord->LetterWord->Bool
matchStemSuffixMap m stem suf = case M.lookup stem m  of
                                       Just (Annotation a f) -> S.member suf a
                                       Nothing -> False
updateStemSuffixMap::StemSuffixMap->(LetterWord,Float)->StemSuffixMap
updateStemSuffixMap mp (wrd,frq) = case filter (uncurry $ matchStemSuffixMap mp) $ zip (inits wrd) (tails wrd) of
                                        [] -> mp
                                        (stem,suf):xs -> M.adjust (\x->Annotation {wordsuffixes = wordsuffixes x, wordfreq = wordfreq x + frq}) stem mp
showStemSuffixMap:: StemSuffixMap->String
showStemSuffixMap = unlines . map (\(f,s)->("sufmap: <"++ map belletter f ++","++ show s++">")) . M.toAscList

makefreqs:: StemSuffixMap->[(LetterWord,Float)]->StemSuffixMap
makefreqs = foldl' updateStemSuffixMap

removeEmptySuf::SuffixSet->SuffixSet
removeEmptySuf = S.delete []
hasEmptySuf:: SuffixSet->Bool
hasEmptySuf  = S.member [] 
enumerateSuffixSets::StemSuffixMap->[(Int,SuffixSet)]
enumerateSuffixSets m = zip [0::Int ..] $ map wordsuffixes $ M.elems m 

suffixLookupTable::StemSuffixMap->M.Map SuffixSet Int
suffixLookupTable  = M.fromList. map swap. enumerateSuffixSets

makeSuffixSets:: StemSuffixMap->AllSetS 
makeSuffixSets m = amap removeEmptySuf $ array (0, length enumeration -1) enumeration where  enumeration = enumerateSuffixSets m

makeDictWords:: StemSuffixMap->[DictWord]
makeDictWords m = map convertpair $ M.toAscList m
   where
     lookuptbl = suffixLookupTable m
     convertpair::([Letter],Annotation)->DictWord
     convertpair (l, a) = DictWord { st = l, wlemma = checkwlemma $ wordsuffixes a, freq = wordfreq a, freeTerm = hasEmptySuf $ wordsuffixes a}
     checkwlemma:: SuffixSet->Int
     checkwlemma s | hasEmptySuf s && S.size s == 1 = -1
                   | otherwise = fromJust $ M.lookup s lookuptbl    

--allcombinations generates all possible path through list of lists
allcombinations :: [[a]]->[[a]]
allcombinations [] = []
allcombinations ([]:xss) = allcombinations xss
allcombinations (lst:[]) = map (:[]) lst
allcombinations (lstx:lstxs) = concatMap (\x->map (x:) $ allcombinations lstxs) lstx

commonPrefix :: Eq a=>[[a]]->([a],[[a]])
commonPrefix [] = ([],[])
commonPrefix lst  | any null lst  = ([],lst)
                  | not $ all (== head ( head lst)) $ map head lst = ([],lst)
                  | otherwise = ( head  ( head lst):prefix, suffixes) 
                                 where (prefix, suffixes) = commonPrefix $ map tail lst

uniq:: Ord a => [a]->[a]
uniq = S.toList . S.fromList


------------------

parseDicFile:: Alphabet->String->[DicFileRec]
parseDicFile a = mapMaybe (parseDicFileEntry a) . lines
parseDicFileEntry::Alphabet->String->Maybe DicFileRec
parseDicFileEntry a s | null strhead = Nothing
                      | isDigit $ head strhead = Nothing
                      | (== '#') $ head strhead = Nothing
                      | isNothing (letters a strhead)  = Nothing
                      | otherwise = Just $ DicFileRec (fromJust $ letters a strhead) keys 
     where
      (strhead, rest) = break (=='/') s
      keys = dropWhile (=='/') rest

---------
parseAffFile:: Alphabet->String->AffixList       
parseAffFile a s = M.fromList $ groupLetterIdx $ getLines a s

--------
parseFreqFile:: Alphabet->String->[(LetterWord,Float)]
parseFreqFile a = map (parseFreqLine a) . lines
parseFreqLine :: Alphabet->String->(LetterWord,Float)
parseFreqLine a = p1  . words where
                p1 [] = ([], 0.0)
                p1 (b:[]) = (fromMaybe [] $ letters a b, 0.0)
                p1 (b:c:_) = (fromMaybe [] $ letters a b, read c)

main = do 
   args <- getArgs
   unless (length args == 3) $ error "Usage hunspelconvert <dicfile> <afffile> <freqfile>"
   diccontent <- readFile $ head args
   affcontent <- readFile $ head (tail args)
   freqcontent <- readFile $ args !! 2
   let sfxlist = mkSfxList $ parseAffFile belarusianAlphabet affcontent
   print ('P' `M.lookup` sfxlist )
   print ('H' `M.lookup` sfxlist )
   print $ joinAllSuffixMaps sfxlist "HP"
   print $ letters' "шыла"
   print $ getRules (joinAllSuffixMaps sfxlist "HP") $ letters' "шыла"
   --let y = parseDicFile belarusianAlphabet diccontent
   let z = map (mkWordNest sfxlist) $ parseDicFile belarusianAlphabet diccontent 
   --print z
   print $ mkWordNest sfxlist $  DicFileRec (letters' "шыла") "HP"
   print "-------------------------------------------------------------------------"
   let frq = parseFreqFile belarusianAlphabet freqcontent
   let q = makefreqs (buildStemSuffixMap z) frq
   let p =  makeSuffixSets q
   --putStrLn $ showStemSuffixMap  q
   putStrLn "xxxx----------------------------------------------------------------------"
   putStrLn $ unlines $ map (\x->show x ++ (if wlemma x >0 then intercalate "," $ map (map belletter) $ S.toAscList  (p ! wlemma x) else "")) $   makeDictWords q

    
   
   
        

