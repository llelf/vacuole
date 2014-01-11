{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import Haste
import Haste.Prim
import Haste.Ajax
import Haste.JSON
import Control.Monad
import Control.Arrow (second)
import qualified Data.IntMap as Map

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
mkNode node = vanillaNode node



nowhereNode = genericNode 3

consNode = genericNode 15


memNode node = do
  p <- paper
  cs <- forM [1..3] $ \x -> circle (x*10,x*10) 20 p
  g <- g p
  foldM (flip append) g cs

vanillaNode = genericNode 20

genericNode size node = do
  p <- paper
  c <- circle (0,0) size p >>= setAttrs [(Class,"c")]
  t <- text (0,0) "@@@" p >>= setAttrs [(TextAnchor,"middle"),
                                              (AlignmentBaseline,"middle")]
  g p >>= append c >>= append t



mkLink :: Link -> Element -> IO Element
mkLink link arrow = do
  p <- paper
  l <- line (0,0) (0,0) p
  setAttrPtr (MarkerEnd,toPtr arrow) l




arrowDef = do
  p <- paper
  a <- path "M0,-5 L15,0 L0,5" p
  setAttrs [(Stroke,"red"), (Fill,"green"),
            (Transform, scale s)] a
  marker (0,-5) (15,10) ((19+15)`div`3,0) a
      where
        alen = 15
        s = 1/3

draw nodesE linksE = do
  p <- paper
  linksG <- g p
  nodesG <- g p
  setAttrs [(Class,"g-links")] linksG
  setAttrs [(Class,"g-nodes")] nodesG
  forM_ nodesE (flip append nodesG)
  forM_ linksE (flip append linksG)


tickN :: Map.IntMap Node -> JSON -> Element -> IO ()
tickN nodes param node = do
  setAttr (Transform, translate (round x) (round y)) node
  return ()
      where
        Num x = param!"x"
        Num y = param!"y"


tickL :: Int->Int->Int->Int -> Element -> IO ()
tickL sx sy tx ty link = do
  ($link) $ setAttrs $ map (second (toJSString.show)) [(X1,x1), (Y1,y1), (X2,x2), (Y2,y2)]
  return ()
    where
      [x1,y1,x2,y2]=[sx,sy,tx,ty]



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
                linksE <- mapM (flip mkLink arrow) links'
                draw nodesE linksE
                drawGraph (toPtr nodesE) (toPtr linksE)
                          (jsonToJS fromTo)
                          (toPtr $ tickN nodesMap)
                          (toPtr tickL)


main = initTerm (toPtr newInput)



