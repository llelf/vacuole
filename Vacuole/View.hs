{-# LANGUAGE TupleSections, OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Vacuole.View (boo) where

import Data.Char
import Data.IntMap.Strict (elems,mapWithKey)
import qualified Data.ByteString.Lazy.Char8 as BS
import Vacuole.Interp
import Vacuole.View.Types
import Data.Aeson.TH
import Data.Aeson

deriveToJSON defaultOptions ''Colour
deriveToJSON defaultOptions ''Node
deriveToJSON defaultOptions ''Link

graph p = object ["nodes" .= nodes p, "links" .= links p]

boo s = do vvv <- vacuumise s
           return $ case vvv of
                      Left e -> object [ "error" .= show e ]
                      Right v -> graph v



node n t = Node {size=10, colour=Blue, name=n, desc=t}

toJS nd @HNode {nodeLits=lits, nodeInfo=info}
    | n=="S#" || n=="I#" = node (show $ head $ lits) "int"
    | n=="C#" = node (show $ chr $ fromIntegral $ head $ lits) "char"
    | n==":"  = node "(:)" ""
    | n=="[]"  = node "[]" ""
    | otherwise = node n "other"
    where n = nodeName nd



nodes = map toJS . elems

links = concat . elems . mapWithKey f
    where f k (HNode {nodePtrs=ls}) = map (\to -> Link k to) ls


