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




paper :: IO Paper
paper = ffi "Paper"


-- parseNodes :: JSON -> [Node]
-- parseNodes (Arr ns) = map parseJSON ns

-- parseLinks :: JSON -> [Link]
-- parseLinks (Arr ls) = map parseJSON ls


foreign import ccall canvasClear :: IO ()

foreign import ccall drawGraph
    :: Ptr [Element] -> Ptr [Element] -> JSAny
    -> Ptr (JSON -> Element -> IO ())
    -> Ptr (Int->Int->Int->Int->Element->IO ())
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



mkLink :: Map.IntMap Node -> Element -> Link -> IO Element
mkLink nodeMap arrow link@(Link from to)
    | n == 1    = mkSimpleLink arrow link
--    | otherwise = mkMultiLink arrow link
    where
      n = 1



mkSimpleLink arrow link = do
  p <- paper
  l <- path "M0,0" p
  setAttr (Class,"link") l
  setAttrPtr (MarkerMid,toPtr arrow) l

-- mkMultiLink arrow link = do
--   p <- paper
  



arrowDef = do
  p <- paper
  a <- path "M0,-7 L15,0 L0,7" p
  setAttrs [(Class,"arrow"), (Transform, scale s)] a
  marker (0,-7) (15,14) (round $ fromIntegral(alen)*s/2,0) a
      where
        alen = 15
        s = 1/3

draw nodesE linksE = do
  p <- paper
  outer <- g p
  linksG <- g p
  nodesG <- g p
  append outer linksG
  append outer nodesG
  setAttrs [(Class,"g-links")] linksG
  setAttrs [(Class,"g-nodes")] nodesG
  forM_ nodesE (append nodesG)
  forM_ linksE (append linksG)


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

tickL :: Int -> Int -> Int -> Int -> Element -> IO ()
tickL sx sy tx ty link = do
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


showGraph g = do
                let (nodes,links) = g :: GraphView

                print g

                let nodesMap = Map.fromList $ zip [1..] nodes
                    nNodes = length nodes
                    links' = filter sane links
                    sane (Link x y) = x < nNodes && y < nNodes

                let fromTo = Arr $
                     map (\(Link s t) -> Arr $ map (Num . fromIntegral) [s,t]) links'

                arrow <- arrowDef

                nodesE <- mapM mkNode nodes
                linksE <- mapM (mkLink nodesMap arrow) links'
                draw nodesE linksE
                drawGraph (toPtr nodesE) (toPtr linksE)
                          (jsonToJS fromTo)
                          (toPtr $ tickN nodesMap)
                          (toPtr tickL)


main = initTerm (toPtr newInput)



