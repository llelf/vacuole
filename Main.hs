{-# LANGUAGE OverloadedStrings #-}
module Main where

import Haste
import Haste.Prim
import Haste.JSON
import Haste.Ajax
import Control.Monad
import Control.Arrow (second)
import qualified Data.IntMap as Map

import Vacuole.Snap
import Vacuole.View.Types


class FromJSON a where
    parseJSON :: JSON -> a


instance FromJSON Node where
    parseJSON o = Node (round size) Red Vanilla (fromJSStr name) (fromJSStr desc)
        where Num size = o!"size"
              Str name = o!"name"
              Str desc = o!"desc"


instance FromJSON Link where
    parseJSON o = Link (round from) (round to)
        where Num from = o!"from"
              Num to = o!"to"


paper :: IO Paper
paper = ffi "Paper"


parseNodes :: JSON -> [Node]
parseNodes (Arr ns) = map parseJSON ns

parseLinks :: JSON -> [Link]
parseLinks (Arr ls) = map parseJSON ls


foreign import ccall canvasClear :: IO ()

foreign import ccall drawGraph
    :: Ptr [Element] -> Ptr [Element] -> JSAny
    -> Ptr (JSON -> Element -> IO ())
    -> Ptr (Int->Int->Int->Int->Element->IO ())
    -> IO ()

foreign import ccall initTerm :: Ptr (JSString -> IO Bool) -> IO ()


mkNode :: Node -> IO Element
mkNode node | k==Vanilla  = vanillaNode node
            | k==ArrWords = memNode node
    where k = kind node

memNode node = do
  p <- paper
  cs <- forM [1..3] $ \x -> circle (x*10,x*10) (size node) p
  g <- g p
  foldM (flip append) g cs

vanillaNode node = do
  p <- paper
  c <- circle (0,0) (size node) p >>= setAttrs [(Class,"c")]
  t <- text (0,0) (name node) p >>= setAttrs [(TextAnchor,"middle"),
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
              jsonRequest_ POST "/vac"
                               [("expr",v)] $ \d -> do
                print d
                putStrLn "will clear now"
                canvasClear
                let Just g = d
                let nodes = parseNodes $ g!"nodes"
                    links = parseLinks $ g!"links"

                let nodesMap = Map.fromList $ zip [1..] nodes

                let fromTo = Arr $
                     map (\(Link s t) -> Arr $ map (Num . fromIntegral) [s,t]) links

                arrow <- arrowDef

                nodesE <- mapM mkNode nodes
                linksE <- mapM (flip mkLink arrow) links
                draw nodesE linksE
                drawGraph (toPtr nodesE) (toPtr linksE)
                          (jsonToJS fromTo)
                          (toPtr $ tickN nodesMap)
                          (toPtr tickL)

              return True


main = initTerm (toPtr newInput)


