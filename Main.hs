{-# LANGUAGE OverloadedStrings #-}
module Main where

import Haste
import Haste.Prim
import Haste.JSON
import Haste.Ajax
import Vacuole.Snap
import Vacuole.View.Types


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


inputValue :: IO JSString
inputValue = ffi "d3.select('textarea').property('value')"

canvasClear :: IO ()
canvasClear = ffi "canvasClear()"

foreign import ccall drawGraph
    :: JSString -> Ptr [Element] -> IO ()


mkNode :: Node -> IO Element
mkNode node = do
  p <- paper
  c <- circle (0,0) (size node) p >>= flip setAttrs [(Class,"c")]
  t <- text (0,0) (name node) p >>= flip setAttrs [(TextAnchor,"middle"),
                                                   (AlignmentBaseline,"middle")]
  g p >>= append c >>= append t



newInput = do v <- inputValue
              print v
              jsonRequest_ POST "/vac"
                               [("expr",v)] $ \d -> do
                print d
                canvasClear
                let Just g = d
                let nodes = parseNodes $ g!"nodes"
                nodesE <- mapM mkNode nodes
                drawGraph (encodeJSON g) (toPtr nodesE)


main = newInput

