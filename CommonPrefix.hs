module CommonPrefix where

import Data.Tree
import Data.List
import Data.Ord
import TreeUtil
--Convert list of strings into list of tuples (common prefix, [suffix]), where the list of suffixes has a length, specified by the first argument
--frequentPrefixes::Int->[[a]]->[([a],[[a]])]
--frequentPrefixes maxsuflen lst = 


--forest = unfoldForest unfolder 
makeForest::Eq a => [[a]]-> Forest a
makeForest lst = unfoldForest unfolder (groupBy fsteq lst)

unfolder::Eq a => [[a]]->(a,[[[a]]])
unfolder lst = (head $ head lst, groupBy fsteq $ removefst lst)  
fsteq::Eq a => [a]->[a]->Bool
fsteq [] [] = True
fsteq [] _ = False
fsteq _ [] = False
fsteq lst1 lst2 = head lst1 == head lst2
removefst::[[a]]->[[a]]
removefst =  map tail . filter (not . null ) 

mapSnd::(a->b)->(c,a)->(c,b)
mapSnd f (x,y) = (x, f y)
mapFst::(a->b)->(a,c)->(b,c)
mapFst f (x,y) = (f x, y)

commonPrefix :: Eq a=>[[a]]->([a],[[a]])
commonPrefix [] = ([],[])
commonPrefix lst  | any null lst  = ([],lst)
                  | not $ all (== head ( head lst)) $ map head lst = ([],lst)
                  | otherwise = ( head  ( head lst):prefix, suffixes) 
                                 where (prefix, suffixes) = commonPrefix $ map tail lst

frequentPrefixes::Ord a=>Int->[[a]]->[([a],[[a]])]
frequentPrefixes thresh lst =  concatMap (mapaddletter. (mapSnd (frequentPrefixes thresh)) .takefstletter) recursivelst ++ map (mapFst (\x->[x]). takefstletter) middlelst ++ [([],concat taillst)]
  where
  --addletter a->([a],[[a]])->([a],[[a]])
  --addletter l t = (l:fst t, snd t)  
  mapaddletter:: (a,[([a],[[a]])])->[([a],[[a]])]
  mapaddletter (x,lst) = map (mapFst (x:)) lst
  takefstletter::[[a]]->(a,[[a]])
  takefstletter strlst = (head $ head strlst, removefst strlst)
  recursivelst = map snd $ takeWhile (\x->fst x > thresh) annotatedlst 
  restlst =  snd $ mapAccumL (\acc->(\x->(acc+fst x, (acc+fst x, snd x)))) 0 $ reverse $ dropWhile (\x-> fst x > thresh) annotatedlst
  taillst =  map snd $ takeWhile (\x->(fst x <= thresh))  $  restlst
  middlelst = map snd $ dropWhile (\x->(fst x <= thresh))  $  restlst
  annotatedlst = reverse $ sortBy (comparing fst) $ map (\x->(length x, x)) $ groupBy fsteq $ sort lst 

{-
countSuffix::a->[(a,Int)]->(a,Int)
countSuffix symbol lst = (symbol, 1+ (sum $  map snd lst))

flattenForest:: Forest a->[[a]]
flattenForest [] = [[]]
flattenForest x = concatMap flattenTree x 

flattenTree:: Tree a -> [[a]]
flattenTree node = map ((rootLabel node):) (flattenForest $ subForest node)

splitForestAtCount::Int->Forest (a, Int) ->[([a],[[a]])]
splitForestAtCount thresh forest = concatMap (splitNodeAtCount thresh) forest

splitNodeAtCount :: Int->Tree (a, Int)->[([a],[[a]])]
splitNodeAtCount thresh node = if (snd $ rootLabel node) < thresh 
            then [( [fst $ rootLabel node], flattenLabeledForest $ subForest node)]
            else map (\x->((fst $ rootLabel node):fst x, snd x)) $ splitForestAtCount thresh $ subForest node  
-}
--mapAccumForest2 countSuffix $ unfoldForest unfolder 

