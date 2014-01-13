{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import Haste
import Haste.Prim
import Haste.Ajax
import Haste.JSON
import Control.Monad
import Control.Arrow (second)
import Control.Applicative
import qualified Data.IntMap as Map
import Text.Printf

import Vacuole.Snap
import Vacuole.View.Types
import Multi



paper :: IO Paper
paper = ffi "Paper"


foreign import ccall canvasClear :: IO ()

foreign import ccall drawGraph
    :: Ptr [Element] -> JSAny
    -> Ptr (JSON -> Element -> IO ())
    -> Ptr (Int->Int->Int->Int->Int->IO ())
    -> IO ()

foreign import ccall initTerm :: Ptr (JSString -> IO Bool) -> IO ()


mkNode :: Node -> IO Element
mkNode (Node (Vanilla s) _) = vanillaNode s
mkNode (Node EmptyList _)  = vanillaNode "[]"
mkNode (Node ArrWords _) = memNode
mkNode (Node Cons _) = consNode
mkNode (Node Fun _)  = vanillaNode "λ"
mkNode (Node Nowhere _) = nowhereNode


nowhereNode = genericNode 3 "xx"

consNode = genericNode 15 "(:)"


memNode = do
  p <- paper
  cs <- forM [3,2..0] $ \x ->
          circle (x*3,x*3) 20 p >>= setAttrs [(Class,"c")]
  g <- g p
  foldM append g cs

vanillaNode = genericNode 20

genericNode size str = do
  p <- paper
  c <- circle (0,0) 17 p >>= setAttrs [(Class,"c")]
  t <- text (0,0) str p >>= setAttrs [(TextAnchor,"middle"),
                                      (AlignmentBaseline,"middle")]
  g p >>= flip append c >>= flip append t



data LinkElem = SingleElem Element
              | MultiElem [Element] Element
                deriving Show


linkOuterElem (SingleElem e) = e
linkOuterElem (MultiElem _ e) = e


mkLink :: Map.IntMap Node -> Element -> HLink -> IO LinkElem
mkLink nodeMap arrow (Single link) = mkSimpleLink arrow link
mkLink nodeMap arrow hlink@Multi{}  = mkMultiLink arrow hlink


mkSimpleLink arrow link = do
  p <- paper
  l <- path "M0,0" p
  setAttr (Class,"link") l
  setAttrPtr (MarkerMid,toPtr arrow) l
  return $ SingleElem l

mkMultiLink arrow (Multi link dirs) = do
  p <- paper
  gr <- g p
  -- let n = length dirs
  -- ps <- sequence $ replicate n $ path "M0,0" p
  return undefined

  



arrowDef = do
  p <- paper
  a <- path "M0,-7 L15,0 L0,7" p
  setAttrs [(Class,"arrow"), (Transform, scale s)] a
  marker (0,-7) (15,14) (round $ fromIntegral(alen)*s/2,0) a
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



data Vec = Vec Double Double

instance Num Vec where
    Vec x y + Vec x' y' = Vec (x+x') (y+y')
    negate (Vec x y) = Vec (-x) (-y)
    (*) = undefined
    signum = undefined
    abs = undefined
    fromInteger = undefined

vecScale a (Vec x y) = Vec (a*x) (a*y)

mkVec :: (Int,Int) -> Vec
mkVec (x,y) = Vec (fromIntegral x) (fromIntegral y)

toInts :: Vec -> (Int,Int)
toInts (Vec x y) = (round x, round y)

tickL :: Map.IntMap LinkElem -> Int -> Int -> Int -> Int
      -> Int -> IO ()
tickL links ix = tickLink $ links Map.! ix

tickLink :: LinkElem -> Int -> Int -> Int -> Int -> IO ()
tickLink (SingleElem link) sx sy tx ty = do
  setAttr (D,d) link
  return ()
    where
--      d0 = toJSStr $ printf "M%d,%d L%d,%d L%d,%d" sx sy mx my tx ty
      d = toJSStr $ printf "M%d,%d Q%d,%d,%d,%d Q%d,%d,%d,%d"
                         sx sy cx cy lmx lmy c1x c1y tx ty
      src = mkVec (sx,sy)
      dst = mkVec (tx,ty)
      mid = vecScale 0.5 (src+dst)
      dir = mid - src
      dirN = vecScale 0.77 $ norm dir
      (lmx,lmy) = toInts $ mid + dirN
      out = -dir + dirN

      out1 =  out
      ctl = src + dirN
      (cx,cy) = toInts ctl

      ctl1 = dst + dirN
      (c1x,c1y) = toInts ctl1

      norm (Vec x y) = Vec (-y) x
      norm1 (Vec x y) = Vec y (-x)


defaultInput = "[1..3]"


newInput :: JSString -> IO Bool
newInput v = do
              print v
              textRequest_ POST "/vac"
                               [("expr",v)] $ \res -> do
                print res
                canvasClear
                case res of
                  Nothing -> showError "Ajax error"
                  Just dat ->
                      case read (fromJSStr dat::String) :: Either String GraphView of
                        Left err -> showError err
                        Right graph  -> showGraph graph

              return True



showError err = alert err


linkEnds (Single link) = link
linkEnds (Multi link _) = link


showGraph g = do
                let (nodes,links0) = g :: GraphView
                    links = linksGatherMulti links0

                print g

                let nodesMap = Map.fromList $ zip [0..] nodes
                    linksMap = Map.fromList $ zip [0..] links

                let fromTo = Arr $
                     map (\(Link s t) -> Arr $ map (Num . fromIntegral) [s,t]) $ map linkEnds $ links


                arrow <- arrowDef

                nodesE <- mapM mkNode nodes
                zoo <- mapM (mkLink nodesMap arrow) links
                let linkElems = Map.fromList $ zip [0..] zoo

                print linkElems

                draw nodesE linkElems
                drawGraph (toPtr nodesE)
                          (jsonToJS fromTo)
                          (toPtr $ tickN nodesMap)
                          (toPtr $ tickL linkElems)


main = initTerm (toPtr newInput)



