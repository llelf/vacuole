{-# LANGUAGE TupleSections, OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Vacuole.View (boo) where

import Data.Char
import Data.IntMap.Strict (elems,mapWithKey,notMember)
import qualified Data.ByteString.Lazy.Char8 as BS
import Vacuole.Interp
import Vacuole.View.Types
import qualified GHC.Vacuum.ClosureType as Closure
import Data.Aeson.TH
import Data.Aeson

deriveToJSON defaultOptions ''Kind
deriveToJSON defaultOptions ''Node
deriveToJSON defaultOptions ''Link


jsonGraph g = object ["nodes" .= nodes, "links" .= links]
    where (nodes,links) = nodesLinks g


boo :: String -> IO Value
boo s = do vvv <- vacuumise s
           return $ case vvv of
                      Left e -> object [ "error" .= show e ]
                      Right v -> jsonGraph v



node n t = Node {kind=Vanilla, name=n, desc=t}

isFun = Closure.isFun . itabType

showChr c | isPrint c = ['\'',c,'\'']
          | otherwise = show c


toJS :: HNode -> Node
toJS nd @HNode {nodeLits=lits, nodeInfo=info}
    | n=="S#" || n=="I#" = node (show $ head $ lits) "int"
    | n=="C#" = node (showChr $ chr $ fromIntegral $ head $ lits) "char"
    | n==":"  = Node Cons n ""
    | n=="[]"  = node "[]" ""
    | isFun info = node "λ" ""
    | itabType info == Closure.ARR_WORDS = (node "" "") {kind=ArrWords}
    | otherwise = node n $ show nd
    where n = nodeName nd


nodesLinks :: Vacuum -> ([Node],[Link])
nodesLinks graph = (nodes ++ nowheres, links)
    where nodes = graphNodes graph
          links = graphLinks graph
          nowheres = nowhereNodes graph links


graphNodes :: Vacuum -> [Node]
graphNodes graph = map toJS . elems $ graph

nowhereNodes :: Vacuum -> [Link] -> [Node]
nowhereNodes graph ls = replicate n $ Node Nowhere "nowhere"
    where n = length . filter (\(Link _ to) -> to `notMember` graph) $ ls


graphLinks :: Vacuum -> [Link]
graphLinks = concat . elems . mapWithKey f
    where f k (HNode {nodePtrs=ls}) = map (\to -> Link k to) ls


