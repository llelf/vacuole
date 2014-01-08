{-# LANGUAGE OverloadedStrings #-}
module Main where

import Haste
import Haste.Prim
import Haste.JSON
import Haste.Ajax
import Vacuole.Snap
import Vacuole.View.Types
import Control.Monad
import Control.Applicative
import qualified Data.IntMap as Map

class FromJSON a where
    parseJSON :: JSON -> a


instance FromJSON Node where
    parseJSON o = Node (round size) Red (fromJSStr name) (fromJSStr desc)
        where Num size = o!"size"
              Str name = o!"name"
              Str desc = o!"desc"


instance FromJSON Link where
    parseJSON o = Link (round from) (round to)
        where Num from = o!"from"
              Num to = o!"to"


jsMain' :: IO ()
jsMain' = ffi "alert('hi')"

jsMain :: IO ()
jsMain = ffi "newInput()"

globalSet :: String -> Int -> IO ()
globalSet var = ffi $ "(function(x){" ++ var ++ "=x; return {}})"


paper :: IO Paper
paper = ffi "Paper"


parseNodes :: JSON -> [Node]
parseNodes (Arr ns) = map parseJSON ns

parseLinks :: JSON -> [Link]
parseLinks (Arr ls) = map parseJSON ls

inputValue :: IO JSString
inputValue = ffi "d3.select('textarea').property('value')"

canvasClear :: IO ()
canvasClear = ffi "canvasClear()"

foreign import ccall drawGraph
    :: Ptr [Element] -> Ptr [Element] -> JSAny -> IO ()


mkNode :: Node -> IO Element
mkNode node = do
  p <- paper
  c <- circle (0,0) (size node) p >>= setAttrs [(Class,"c")]
  t <- text (0,0) (name node) p >>= setAttrs [(TextAnchor,"middle"),
                                              (AlignmentBaseline,"middle")]
  g p >>= append c >>= append t



mkLink :: Link -> IO Element
mkLink link = do
  p <- paper
  line (0,0) (0,0) p




arrowDef = do
  p <- paper
  a <- path "M0,-5 L15,0 L0,5" p
  setAttrs [(Stroke,"red"), (Fill,"green"), (Transform,"scale(0.333)")] a
  marker (0,-5) (15,10) (10,0) a


draw nodesE linksE = do
  p <- paper
  linksG <- g p
  nodesG <- g p
  setAttrs [(Class,"g-links")] linksG
  setAttrs [(Class,"g-nodes")] nodesG
  forM_ nodesE (flip append nodesG)
  forM_ linksE (flip append linksG)



newInput = do v <- inputValue
              print v
              jsonRequest_ POST "/vac"
                               [("expr",v)] $ \d -> do
                print d
                canvasClear
                let Just g = d
                let nodes = parseNodes $ g!"nodes"
                    links = parseLinks $ g!"links"

                let nodesMap = Map.fromList $ zip [1..] nodes

                let fromTo = Arr $
                     map (\(Link s t) -> Arr $ map (Num . fromIntegral) [s,t]) links

                nodesE <- mapM mkNode nodes
                linksE <- mapM mkLink links
                draw nodesE linksE
                drawGraph (toPtr nodesE) (toPtr linksE)
                          (jsonToJS fromTo)


main = newInput

