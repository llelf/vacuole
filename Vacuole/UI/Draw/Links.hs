{-# LANGUAGE OverloadedStrings #-}
module Vacuole.UI.Draw.Links where

import Vacuole.View.Types
import Vacuole.UI.Multi
import Vacuole.Snap
import Vacuole.UI.Draw.Paper
import Haste.Prim

import qualified Data.IntMap as Map
import Control.Monad (forM_)


data LinkElem = SingleElem Element          -- ^ single link
              | MultiElem [Element] Element -- ^ multi-link
              | SelfElem                    -- ^ self-link
                deriving Show


linkOuterElem (SingleElem e) = e
linkOuterElem (MultiElem _ e) = e



mkLink :: Map.IntMap Node -> Element -> HLink -> IO LinkElem
mkLink nodeMap arrow (Single link) = mkSimpleLink arrow link
mkLink nodeMap arrow hlink@Multi{}  = mkMultiLink arrow hlink


linkPath pap arrow = do
  l <- path "M0,0" pap >>= setAttr (Class,"link")
  setAttrPtr (MarkerMid, toPtr arrow) l
  return l

mkSimpleLink arrow link = do
  p <- paper
  l <- linkPath p arrow
  return $ SingleElem l

mkMultiLink arrow (Multi link dirs) = do
  p <- paper
  gr <- g p
  ps <- sequence $ replicate (length dirs) $ linkPath p arrow
  forM_ ps $ append gr
  return $ MultiElem ps gr




arrowDef = do
  p <- paper
  a <- path "M0,-7 L15,0 L0,7" p
  setAttrs [(Class,"arrow"), (Transform, scale s)] a
  marker (0,-7) (15,14) (round $ fromIntegral alen * s / 2, 0) a
      where
        alen = 15
        s = 1/3



linkEnds (Single link) = link
linkEnds (Multi link _) = link

