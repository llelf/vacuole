module Vacuole.UI.Multi where

import Vacuole.View.Types
import qualified Data.Map as M
import Data.List

data HLink = Single Link
           | Self
           | Multi Link [Bool]


norm (Link a b) = Link (min a b) (max a b)



linksGatherMulti :: [Link] -> [HLink]

linksGatherMulti [] = []
linksGatherMulti links = map toHLink . groupBy f . sort $ links
    where
      f (Link a b) (Link a' b') | (a,b)==(a',b') || (a,b)==(b',a') = True
                                | otherwise                        = False
      toHLink [x]  = Single x
      toHLink many@(one:_) = Multi one $ map (==one) many


