module CommonType where

import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as B
import Data.Maybe
import Data.Word
import Data.Bits
import Data.Array.IArray
import Data.Set (Set)
import qualified Data.Set as Set

mapfrombel = Map.fromList $ zip [0::Int .. 32::Int] "абвгдеёжзійклмнопрстуўфхцчшыьэюя'"
belletter lt = Map.findWithDefault '-' lt mapfrombel
showhex::Word8->String
showhex a = hexdigit (shiftR a 4) ++ hexdigit (a .&. 0x0f)
hexdigit::Word8->String
hexdigit a 
          | a<10 = show a
          | a==10 = "a"
          | a==11 = "b"
          | a==12 = "c"
          | a==13 = "d"
          | a==14 = "e"
          | a==15 = "f"            
showhexstring::B.ByteString->String
showhexstring = concatMap showhex . B.unpack

type Letter = Int
type Letters = [Letter]

type SetS = Set Letters

type AllSetS = Array Int SetS

data DictWord = DictWord { st::[Letter], wlemma::Int, freq::Float, freeTerm::Bool}
instance Show DictWord where
 show (DictWord st wl fr freeterm) = map belletter st ++ " lemma: "++show wl++" frequency: "++show fr++" terminal: "++show freeterm

type Alphabet = Map Char Int
russianBig = ['А' .. 'Я' ] 
russianSmall = ['а' .. 'я']
russianAlphabet::Alphabet
russianAlphabet = Map.fromList $ zip russianBig [0::Int .. 31::Int] ++ zip russianSmall [0::Int .. 31::Int]

belarusianBig = "АБВГДЕЁЖЗІЙКЛМНОПРСТУЎФХЦЧШЫЬЭЮЯ"
belarusianSmall = "абвгдеёжзійклмнопрстуўфхцчшыьэюя'"
belarusianAlphabet = Map.fromList $ zip belarusianBig [0::Int .. 31::Int] ++ zip belarusianSmall [0::Int .. 32::Int]

letters:: Alphabet->String -> Maybe [Letter]
letters abc = mapM (`Map.lookup` abc)  

letters' w = fromMaybe [] $ letters belarusianAlphabet w

data AffRule = Pfx { rule:: ReplRule, match :: RegexRule } | Sfx { rule:: ReplRule, match :: RegexRule  } | OtherAff 

data ReplRule = ReplRule { matchGroup:: [Letter], replacementGroup:: [Letter] } 

instance Show ReplRule where
   show r = "< match: "++map belletter (matchGroup r)++" repl: "++map belletter (replacementGroup r)++ ">"

showmatch  = concatMap (wrapbr . map belletter)
wrapbr s = "[" ++ s ++ "]"

instance Show AffRule where
   show (Pfx rule match) = "Pfx { "++show rule++", "++showmatch match ++"} "
   show (Sfx rule match) = "Sfx { "++show rule++", "++showmatch match ++"} "
   show OtherAff = "OtherAff"
type RegexRule = [[Letter]]

type LetterIdx = Char
type LetterWord = [Letter]
