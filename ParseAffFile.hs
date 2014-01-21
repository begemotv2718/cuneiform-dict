module ParseAffFile where

import Text.Parsec.Char
import Text.Parsec.String
import Text.Parsec.Combinator
import Text.Parsec.Prim
--import Text.Parsec.Token
import CommonType
import Data.Maybe
import Data.Map as M hiding (map, mapMaybe)
import Data.List
import Data.Function
----for main----
--import System.Environment
--import Control.Monad



groupLetterIdx:: [(LetterIdx,AffRule)]->[(LetterIdx,[AffRule])]
groupLetterIdx = map (headfst.unzip) . groupBy ( (==) `on` fst) . sortBy (compare `on` fst)
              where
                 headfst (a,b) = (head a, b)
getLines :: Alphabet->String->[(LetterIdx,AffRule)]
either2Maybe :: Either b a -> Maybe a
either2Maybe (Right a) = Just a
either2Maybe (Left _) = Nothing
getLines alph = mapMaybe (either2Maybe. parse (pAffFileLine alph) "") . lines

pAffFileLine:: Alphabet-> Parser (LetterIdx, AffRule)
pAffFileLine alph = try (pPfx alph) <|> try (pSfx alph) 

pPfx :: Alphabet->Parser (LetterIdx,AffRule)

pPfx alph = do 
       string "PFX"
       (letter,replRule,regex)<-pRules alph
       return (letter, Pfx replRule regex)

pSfx alph = do
       string "SFX"
       (letter,replRule,regex)<-pRules alph
       return (letter, Sfx replRule regex)

pRules alph = do
       spaces
       letter <- oneOf $ ['A'..'Z']++['a'..'z']
       spaces
       replRule <-pReplRule alph
       spaces
       regex <- pRegexRule alph
       return (letter,replRule,regex)

fromAlphabet:: Alphabet->String->[Letter]
fromAlphabet alph  = mapMaybe (`M.lookup` alph)

pReplRule alph = do
       totake<-many1 $ oneOf ('0': keys alph )
       spaces
       replacement<-many1 $ oneOf $ keys alph
--hack alert: '0' is mapped into [] automatically
       return (ReplRule (fromAlphabet alph totake) (fromAlphabet alph replacement))

pRegexRule:: Alphabet->Parser RegexRule       
pRegexRule alp =  many (pRegexRuleElement alp)

pRegexRuleElement:: Alphabet->Parser [Letter]
pRegexRuleElement alp = pLetterAsList alp <|> pRegexList alp <|> pRegexDot alp

pLetterAsList alph = do
                     letter <- oneOf $ keys alph
                     return (fromAlphabet alph [letter])
pRegexList alph = do
                  chars <- between ( string "[" ) ( string "]") (many $ noneOf "[]")
                  return (fromAlphabet alph chars)

pRegexDot alph = do
                  string "."
                  return (uniq $ M.elems alph)
                  where uniq [] = []
                        uniq a = (map head . group .sort) a
                     


 
               
       
