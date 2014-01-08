{-# LANGUAGE TupleSections, OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Vacuole.View (boo) where

import Data.Char
import Data.IntMap.Strict (elems,mapWithKey)
import qualified Data.ByteString.Lazy.Char8 as BS
import Vacuole.Interp
import Vacuole.View.Types
import qualified GHC.Vacuum.ClosureType as Closure
import Data.Aeson.TH
import Data.Aeson

deriveToJSON defaultOptions ''Colour
deriveToJSON defaultOptions ''Kind
deriveToJSON defaultOptions ''Node
deriveToJSON defaultOptions ''Link

graph p = object ["nodes" .= nodes p, "links" .= links p]

boo s = do vvv <- vacuumise s
           return $ case vvv of
                      Left e -> object [ "error" .= show e ]
                      Right v -> graph v


normalSize = 20

node n t = Node {size=normalSize, colour=Blue, kind=Vanilla, name=n, desc=t}

isFun = Closure.isFun . itabType

toJS nd @HNode {nodeLits=lits, nodeInfo=info}
    | n=="S#" || n=="I#" = node (show $ head $ lits) "int"
    | n=="C#" = node (show $ chr $ fromIntegral $ head $ lits) "char"
    | n==":"  = Node 10 Red Vanilla n ""
    | n=="[]"  = node "[]" ""
    | isFun info = node "λ" ""
    | itabType info == Closure.ARR_WORDS = node "" "" -- {kind=ArrWords}
    | otherwise = node n $ show nd
    where n = nodeName nd



nodes = map toJS . elems

links = concat . elems . mapWithKey f
    where f k (HNode {nodePtrs=ls}) = map (\to -> Link k to) ls


