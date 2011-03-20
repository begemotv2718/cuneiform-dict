import Prelude hiding (foldr)
import Data.Tree
import Data.Traversable
import Data.Foldable hiding (concat)

t a = unfoldTree (\i -> (i,[ i`div`2 .. i-1])) a
accum:: Int -> Int-> (Int,Int)
accum acc i = (acc+1, acc)
folder::Int->String->String
folder a b = "f("++show(a)++", "++b++")"

mapAccumTree::(a->[b]->(b,c))->Tree a -> (b, Tree c)
mapAccumTree f (Node a []) = (b', Node c' [])
           where (b',c') = f a []
mapAccumTree f (Node a sf) = (b', Node c' cs)
           where (b',c') = f a bs
                 (bs,cs) = unzip $ map (mapAccumTree f) sf

accum2:: Int->[String]->(String,String)
accum2 a sts = (show a++" "++ concat sts,show a ++ " "++concat sts)

fmapForest::([a]->[b])->Forest a->Forest b
fmapForest f [] = []
fmapForest f forest = zipWith Node (f rootlabels) (map (fmapForest f) subforests)
           where rootlabels = map rootLabel forest
                 subforests = map subForest forest

marklast::[String]->[String]
marklast [] = []
marklast a = init a ++ [ last a ++ "lst"]

main = do
  putStrLn $ drawTree $ fmap show $ t 6 
  putStrLn $ drawTree $ fmap show $ snd . mapAccumL accum 0 $ t 6 
  putStrLn $ drawTree $ fmap show $ snd . mapAccumR accum 0 $ t 6 
  putStrLn $ foldr folder "" $ t 6
  putStrLn $ drawTree. snd $ mapAccumTree accum2 $ t 6
  putStrLn $ drawForest $ fmapForest marklast $ map (fmap show) $ map t [2..6]
  --putStrLn $ drawForest $ map (fmap show) $ map t [2..6]
