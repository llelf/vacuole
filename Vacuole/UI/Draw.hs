{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ParallelListComp #-}
module Vacuole.UI.Draw (showGraph, canvasClear,
                        linkStrength) where

import Haste (ffi)
import Haste.Prim
import Haste.JSON
import Control.Monad
import Text.Printf
import Data.List ((\\))
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



-- | Each tick for every node
tickN :: Map.IntMap Node -> JSON -> Element -> IO ()
tickN _ param node = do
  setAttr (Transform, translate (round x) (round y)) node
  return ()
      where
        Num x = param!"x"
        Num y = param!"y"


-- | Each tick for every link
tickL :: Map.IntMap HLink -> Map.IntMap LinkElem
      -> Int
      -> Int -> Int -> Int -> Int -> IO ()
tickL links linkEls ix = tickLink (links Map.! ix) (linkEls Map.! ix)

tickLink :: HLink -> LinkElem -> Int -> Int -> Int -> Int -> IO ()

tickLink _ (SingleElem link) sx sy tx ty = do
  setAttr (D,d) link
  return ()
    where
      -- Actually this is not needed now. tickLink (Multi …) is general enough
      d = toJSStr $ printf "M%d,%d L%d,%d L%d,%d" sx sy mx my tx ty
      src = Vec.mkVec (sx,sy)
      dst = Vec.mkVec (tx,ty)
      (mx,my) = Vec.coords $ Vec.scale 0.5 (src+dst)


tickLink (Multi _ dirs) (MultiElem links _) sx sy tx ty
    = sequence_ [ setAttr (D, toJSStr $ pathSpec' f dir) link
                  | link <- links | dir <- dirs | f <- fs (length dirs) ]
    where
      pathSpec' f True  = multiLinkPathSpec f src dst
      pathSpec' f False = multiLinkPathSpec (-f) src dst
      src = (sx,sy)
      dst = (tx,ty)



-- | n of links in multilink -> list of max deviations from base line
fs :: Int -> [Int]
fs n | odd n     = [-n2..n2]
     | otherwise = fs (n+1) \\ [0]
    where n2 = n `div` 2


-- | pathSpec = bezier curve going max to f from base line
multiLinkPathSpec :: Int -> (Int,Int) -> (Int,Int) -> String
multiLinkPathSpec f (sx,sy) (tx,ty)
    = printf "M%d,%d Q%d,%d,%d,%d Q%d,%d,%d,%d"
      sx sy cx cy lmx lmy c1x c1y tx ty
    where
      src = Vec.mkVec (sx,sy)
      dst = Vec.mkVec (tx,ty)
      mid = Vec.scale 0.5 (src+dst)
      dir = mid - src
      dirN = Vec.scale (0.77 * fromIntegral f) $ Vec.rotL dir
      (lmx,lmy) = Vec.coords $ mid + dirN
      out = -dir + dirN

      out1 =  out
      ctl = src + dirN
      (cx,cy) = Vec.coords ctl

      ctl1 = dst + dirN
      (c1x,c1y) = Vec.coords ctl1





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


