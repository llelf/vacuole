{-# LANGUAGE TupleSections #-}
module Vacuole.View where

import Data.Char
import Data.IntMap.Strict (elems,mapWithKey)

import Vacuole.Interp

data Colour = Green | Blue | Red
              deriving Show


data Node = Node {
      size::Int, colour::Colour, name::String, desc::String
} deriving Show

graph s = do Right v <- vacuumise s
             return (nodes v, links v)


node n t = Node {size=10, colour=Blue, name=n, desc=t}

toJS nd @HNode {nodeLits=lits, nodeInfo=info}
    | n=="S#" || n=="I#" = node (show $ head $ lits) "int"
    | n=="C#" = node (show $ chr $ fromIntegral $ head $ lits) "char"
    | n==":"  = node "(:)" ""
    | n=="[]"  = node "[]" ""
    where n = nodeName nd

-- data E = A{a::Int} | B{b::Int}
--          deriving Show


nodes = map toJS . elems

links = concat . elems . mapWithKey (\k (HNode {nodePtrs=ls}) -> map (k,) ls)


