{-# LANGUAGE OverloadedStrings #-}
module Vacuole.UI.Draw (showGraph, canvasClear,
                        linkStrength) where

import Haste (ffi)
import Haste.Prim
import Haste.JSON
import Control.Monad
import Text.Printf
import qualified Data.IntMap as Map

import Vacuole.View.Types
import qualified Vacuole.UI.Vec as Vec
import Vacuole.UI.Multi
import Vacuole.UI.Draw.Links
import Vacuole.UI.Draw.Paper
import Vacuole.UI.Draw.Nodes
import Vacuole.Snap

foreign import ccall canvasClear :: IO ()

foreign import ccall drawGraph
    :: Ptr [Element]                        -- ^ nodesS
    -> JSAny                                -- ^ fromTo
    -> Ptr (JSON -> Element -> IO ())       -- ^ tickN
    -> Ptr (Int->Int->Int->Int->Int->IO ()) -- ^ tickL
    -> IO ()



linkStrength _ = 0.6


draw :: [Element] -> Map.IntMap LinkElem -> IO ()
draw nodesE linkElems = do
  let linksE = Map.elems linkElems
  p <- paper
  outer <- g p
  linksG <- g p
  nodesG <- g p
  append outer linksG
  append outer nodesG
  setAttrs [(Class,"g-links")] linksG
  setAttrs [(Class,"g-nodes")] nodesG
  forM_ nodesE (append nodesG)
  forM_ linksE (append linksG . linkOuterElem)




showGraph g = do
  let (nodes,links0) = g :: GraphView
      links = linksGatherMulti links0

  print links

  let nodesMap = Map.fromList $ zip [0..] nodes
      linksMap = Map.fromList $ zip [0..] links

  let fromTo = Arr $
               map (\(Link s t) -> Arr $ map (Num . fromIntegral) [s,t]) $ map linkEnds $ links

  arrow <- arrowDef

  pap <- paper

  nodesE <- mapM (flip mkNode pap) nodes
  zoo <- mapM (mkLink nodesMap arrow) links
  let linkElems = Map.fromList $ zip [0..] zoo

  print linkElems

  draw nodesE linkElems
  drawGraph (toPtr nodesE) (jsonToJS fromTo)
            (toPtr $ tickN nodesMap)
            (toPtr $ tickL linksMap linkElems)


