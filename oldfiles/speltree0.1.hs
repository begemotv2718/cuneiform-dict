import List
type Letter = Int
data DictNode = DictNode { letter:: Letter, account::Float, terminal::Bool,  subnodes::[DictNode] } deriving (Eq,Show)
{--Properties: Nodes are arranged alphabetically with last letters first  --}
type DictTree = [DictNode]

isEmpty::[Letter] -> Bool
isEmpty = null

updateTree:: [Letter] -> Float -> DictTree -> DictTree
updateTree [] freq tree = tree
updateTree (lt:tail) freq [] = [DictNode { letter=lt, account = freq, terminal = isEmpty tail, subnodes = updateTree tail freq []} ]
updateTree (lt:tail) freq (node:nodes) =
-- Three cases lt == letter node, then update node
   if lt == letter node
       then (updateNode node freq tail):nodes
       else if lt < letter node
            then node:(updateTree (lt:tail)  freq nodes)
            else (newNode (lt:tail) freq):node:nodes

newNode:: [Letter]->Float->DictNode
newNode (lt:tail) freq =  DictNode{
                     letter = lt
                    ,account = freq
                    ,terminal = isEmpty tail
                    ,subnodes = updateTree tail freq []
                                  }

updateNode:: DictNode->Float->[Letter]->DictNode
updateNode nd freq wordrest  = DictNode{ letter = letter nd 
                                        ,account = freq + account nd
                                        ,terminal = isEmpty wordrest || terminal nd
                                        ,subnodes = updateTree wordrest freq $ subnodes nd
                                       } 
