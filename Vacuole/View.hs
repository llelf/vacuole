{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Vacuole.View (boo) where

import Data.Char
import Data.IntMap.Strict (elems,mapWithKey,notMember)
import qualified Data.ByteString.Lazy.Char8 as BS
import Vacuole.Interp
import qualified Vacuole.Interp.Mu as Mu

import Vacuole.View.Types
import qualified GHC.Vacuum.ClosureType as Closure (isFun)
import GHC.Vacuum.ClosureType


boo :: String -> IO (Either String GraphView)
boo s = do vvv <- vacuumise s
           return $ case vvv of
                      Left e  -> Left $ show e
                      Right v -> Right $ nodesLinks v




isFunc = Closure.isFun . itabType

showChr c | isPrint c = ['\'',c,'\'']
          | otherwise = show c


isArrPtrs c = itabType c `elem`
          [MUT_ARR_PTRS_CLEAN,MUT_ARR_PTRS_DIRTY,
           MUT_ARR_PTRS_FROZEN0,MUT_ARR_PTRS_FROZEN,
           MUT_ARR_PTRS_FROZEN]


toJS :: HNode -> Node
toJS node@HNode{nodeLits=lits, nodeInfo=info}
    | n=="S#" || n=="I#"         = Node (Vanilla . show . head $ lits) "int"
    | n=="C#"                    = Node (Vanilla . showChr . chr . fromIntegral . head $ lits) "char"
    | n==":"                     = Node Cons "(:)"
    | n=="[]"                    = Node EmptyList "[]"
    | isFunc info                = Node Fun "λ"
    | itabType info == ARR_WORDS = Node ArrWords "arrwords"
    | isArrPtrs info             = Node ArrPtrs "arrpt"
    | otherwise                  = Node (Vanilla n) . show $ itabType info
    where n = nodeName node


nodesLinks :: Vacuum -> GraphView
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


