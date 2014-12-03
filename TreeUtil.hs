module TreeUtil where
import Data.Tree

mapAccumTree::(a->[b]->(b,c))->Tree a -> (b, Tree c)
mapAccumTree f (Node a []) = (b', Node c' [])
           where (b',c') = f a []
mapAccumTree f (Node a sf) = (b', Node c' cs)
           where (b',c') = f a bs
                 (bs,cs) = unzip $ map (mapAccumTree f) sf

mapAccumTree2::(a->[b]->b)->Tree a->Tree b
mapAccumTree2 f = snd . mapAccumTree (dupl . f) where dupl f x = (f x,f x)

mapAccumForest2::(a->[b]->b)->Forest a -> Forest b
mapAccumForest2 f = map ( mapAccumTree2 f)

fmapForest::([a]->[b])->Forest a->Forest b
fmapForest f [] = []
fmapForest f forest = zipWith Node (f rootlabels) (map (fmapForest f) subforests)
           where rootlabels = map rootLabel forest
                 subforests = map subForest forest

