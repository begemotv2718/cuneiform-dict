--What we have (baseform, [LetterIndex]) (dic file) + (LetterIndex, [AffRule]) (aff file). This is what hunspell provides
--We will likely have the following additional file (word, freq)
--Likely we will have the following intermediate form [(Stem, [Suffix])], and [(Stem,freq,[Suffix])]
--We will need to convert [(Stem,freq,[Suffix])] into [(Stem,freq,SuffixIndex)]+Array SuffixIndex SuffixSet
import Data.Array.IArray
import qualified Data.Map as M
import Data.Maybe

type Letter = Int
type LetterIdx = Char
type LetterWord = [Letter]
data AffRule = Pfx { rule:: ReplRule, match :: RegexRule } | Sfx { rule:: ReplRule, match :: RegexRule  } | OtherAff
data ReplRule = ReplRule { matchGroup:: [Letter], replacementGroup:: [Letter] }
type RegexRule = [[Letter]]
type AffixList = Array LetterIdx [AffRule] -- mapping between letter index and affix rule. This is a result of parsing aff file
data DicFileRec = DicFileRec { baseform:: LetterWord, modifiers:: [LetterIdx] } -- record in a dic file.
data WordNest = WordNest { stem:: [Letter], suffixes :: [[Letter]] } 
data AnnotatedNest = AnnotatedNest { wordnest :: WordNest, frequ :: Float }

--Data STreePlus a b = Node [ EdgePlus a b] | Leaf b
--Data EdgePlus a b = (Label a, StreePlus a b)

type SuffixMap = M.Map LetterWord ReplRule -- we wand some kind of  

mkSuffixMap:: AffRule->Maybe SuffixMap
mkSuffixMap (Sfx r m) = Just (M.fromList $ map (\x->(x,r)) $ allcombinations m)    
mkSuffixMap _ = Nothing
mkSuffixMaps :: [AffRule]->SuffixMap
mkSuffixMaps = M.unions . map fromJust . filter isJust . map mkSuffixMap   

type SfxList = Array LetterIdx SuffixMap
mkSfxList :: AffixList->SfxList
mkSfxList = amap mkSuffixMaps 

allcombinations :: [[a]]->[[a]]
allcombinations [] = []
allcombinations ([]:xss) = allcombinations xss
allcombinations (lst:[]) = map (:[]) lst
allcombinations (lstx:lstxs) = concatMap (\x->map (x:) $ allcombinations lstxs) lstx
