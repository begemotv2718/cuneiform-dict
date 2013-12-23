--What we have (baseform, [LetterIndex]) (dic file) + (LetterIndex, [AffRule]) (aff file). This is what hunspell provides
--We will likely have the following additional file (word, freq)
--Likely we will have the following intermediate form [(Stem, [Suffix])], and [(Stem,freq,[Suffix])]
--We will need to convert [(Stem,freq,[Suffix])] into [(Stem,freq,SuffixIndex)]+Array SuffixIndex SuffixSet
import Data.Array

type LetterIdx = Char
type LetterWord = [Letter]
Data AffRule = Pfx { startGroup:: [Letter], replacement :: [Letter], match :: RegexRule } | Sfx { startGroup:: [Letter], replacement :: [Letter], match :: RegexRule  } | OtherAff
Data RegexRule = [[Letter]]
Data AffixMap = Array LetterIdx [AffRule] -- mapping between letter index and affix rule. This is a result of parsing aff file
Data DicFileRec = DicFileRec { baseform:: LetterWord, modifiers:: [LetterIdx] } -- record in a dic file.
Data WordNest = WordNest { stem:: [Letter], suffixes :: [[Letter]] } 
Data AnnotatedNest = AnnotatedNest { wordnest :: WordNest, frequ :: Float }

Data STreePlus a b = Node [ EdgePlus a b] | Leaf b
Data EdgePlus a b = (Label a, StreePlus a b)

Data AffixTree = STreePlus Letter ReplRule -- we wand some kind of  
Data ReplRule = ReplRule { matchGroup:: [Letter], replacementGroup:: [Letter] }
