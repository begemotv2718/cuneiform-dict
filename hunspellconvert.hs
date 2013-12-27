--What we have (baseform, [LetterIndex]) (dic file) + (LetterIndex, [AffRule]) (aff file). This is what hunspell provides
--We will likely have the following additional file (word, freq)
--Likely we will have the following intermediate form [(Stem, [Suffix])], and [(Stem,freq,[Suffix])]
--We will need to convert [(Stem,freq,[Suffix])] into [(Stem,freq,SuffixIndex)]+Array SuffixIndex SuffixSet
import Data.Array.IArray
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe

type Letter = Int
type LetterIdx = Char
type LetterWord = [Letter]
data AffRule = Pfx { rule:: ReplRule, match :: RegexRule } | Sfx { rule:: ReplRule, match :: RegexRule  } | OtherAff
data ReplRule = ReplRule { matchGroup:: [Letter], replacementGroup:: [Letter] }
type RegexRule = [[Letter]]
type AffixList = Array LetterIdx [AffRule] -- mapping between letter index and affix rule. This is a result of parsing aff file
data DicFileRec = DicFileRec { baseform:: LetterWord, modifiers:: [LetterIdx] } -- record in a dic file.
data WordNest = WordNest { stem:: [Letter], suffixes :: SuffixSet } 
type SuffixSet = S.Set [Letter]

--Data STreePlus a b = Node [ EdgePlus a b] | Leaf b
--Data EdgePlus a b = (Label a, StreePlus a b)

type SuffixMap = M.Map LetterWord [ReplRule] -- we wand some kind of  

mkSuffixMap:: AffRule->Maybe SuffixMap
mkSuffixMap (Sfx r m) = Just (M.fromList $ map (\x->(x,[r])) $ allcombinations m)    
mkSuffixMap _ = Nothing
mkSuffixMaps :: [AffRule]->SuffixMap
mkSuffixMaps = M.unionsWith (++) . mapMaybe mkSuffixMap   

type SfxList = Array LetterIdx SuffixMap
mkSfxList :: AffixList->SfxList
mkSfxList = amap mkSuffixMaps 

joinAllSuffixMaps::SfxList->[LetterIdx]->SuffixMap
joinAllSuffixMaps slst idxs = M.unionsWith (++) $ map (slst ! ) idxs 

getRules::SuffixMap->LetterWord->[ReplRule]
getRules m w = case dropWhile isNothing $ map (`M.lookup` m ) $ tails w 
                      of [] -> []
                         Just lst:lsts -> lst
                         _ -> []

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



data Annotation = Annotation { wordsuffixes::SuffixSet, wordfreq::float}
instance Monoid Annotation where
  mempty = Annotation  (S.empty:: S.Set [Letter]) 0.0  
  Annotation a1 b1 `mappend` Annotation a2 b2 = Annotation (mappend a1 a2) (mappend b1 b2) 

type StemSuffixMap = M.Map LetterWord Annotation
buildStemSuffixMap:: [WordNest]->StemSuffixMap
buildStemSuffixMap wds = M.unionsWith mappend $ map (\x->M.singleton (stem x) (Annotation (suffixes x) 0.0)) wds 

updateStemSuffixMap::StemSuffixMap->[([LetterWord,Float)]->StemSuffixMap


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
