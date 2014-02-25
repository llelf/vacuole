{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ParallelListComp #-}
module Vacuole.UI.Draw where

import Vacuole.Snap
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

data LinkElem = SingleElem Element
              | MultiElem [Element] Element
                deriving Show


linkOuterElem (SingleElem e) = e
linkOuterElem (MultiElem _ e) = e




paper :: IO Paper
paper = ffi "Paper"


foreign import ccall canvasClear :: IO ()

foreign import ccall drawGraph
    :: Ptr [Element] -> JSAny
    -> Ptr (JSON -> Element -> IO ())
    -> Ptr (Int->Int->Int->Int->Int->IO ())
    -> IO ()



linkStrength _ = 0.5


mkNode :: Node -> Paper -> IO Element
mkNode (Node (Vanilla s) _) = vanillaNode s
mkNode (Node EmptyList _)  = vanillaNode "[]"
mkNode (Node ArrWords _) = memNode
mkNode (Node ArrPtrs _) = memNode
mkNode (Node Cons _) = consNode
mkNode (Node Fun _)  = vanillaNode "λ"
mkNode (Node Nowhere _) = nowhereNode


nowhereNode = genericNode 3 "xx"

consNode = genericNode 15 "(:)"


memNode p = do
  cs <- forM [3,2..0] $ \x ->
          circle (x*3,x*3) 20 p >>= setAttrs [(Class,"c")]
  g <- g p
  foldM append g cs

vanillaNode = genericNode 20

genericNode size str p = do
  c <- circle (0,0) 17 p >>= setAttrs [(Class,"c")]
  t <- text (0,0) str p >>= setAttrs [(TextAnchor,"middle"),
                                      (AlignmentBaseline,"middle")]
  g p >>= flip append c >>= flip append t

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



tickN :: Map.IntMap Node -> JSON -> Element -> IO ()
tickN nodes param node = do
  setAttr (Transform, translate (round x) (round y)) node
  return ()
      where
        Num x = param!"x"
        Num y = param!"y"



tickL :: Map.IntMap HLink -> Map.IntMap LinkElem
      -> Int
      -> Int -> Int -> Int -> Int -> IO ()
tickL links linkEls ix = tickLink (links Map.! ix) (linkEls Map.! ix)

tickLink :: HLink -> LinkElem -> Int -> Int -> Int -> Int -> IO ()

tickLink _ (SingleElem link) sx sy tx ty = do
  setAttr (D,d) link
  return ()
    where
      d = toJSStr $ printf "M%d,%d L%d,%d L%d,%d" sx sy mx my tx ty
      src = Vec.mkVec (sx,sy)
      dst = Vec.mkVec (tx,ty)
      (mx,my) = Vec.coords $ Vec.scale 0.5 (src+dst)


tickLink (Multi _ dirs) (MultiElem links _) sx sy tx ty
    = sequence_ [ setAttr (D, toJSStr $ pathSpec' f dir) link
                  | link <- links | dir <- dirs | f <- fs (length dirs) ]
    where
      pathSpec' f True  = pathSpec f src dst
      pathSpec' f False = pathSpec (-f) src dst
      src = (sx,sy)
      dst = (tx,ty)

fs n | odd n     = [-n2..n2]
     | otherwise = fs (n+1) \\ [0]
    where n2 = n `div` 2

pathSpec f (sx,sy) (tx,ty)
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




linkEnds (Single link) = link
linkEnds (Multi link _) = link




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


