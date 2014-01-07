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

parseLinks :: JSON -> [Link]
parseLinks (Arr ls) = map parseJSON ls

inputValue :: IO JSString
inputValue = ffi "d3.select('textarea').property('value')"

canvasClear :: IO ()
canvasClear = ffi "canvasClear()"

foreign import ccall drawGraph
    :: JSString -> Ptr [Element] -> Ptr [Element] -> JSAny -> IO ()


mkNode :: Node -> IO Element
mkNode node = do
  p <- paper
  c <- circle (0,0) (size node) p >>= flip setAttrs [(Class,"c")]
  t <- text (0,0) (name node) p >>= flip setAttrs [(TextAnchor,"middle"),
                                                   (AlignmentBaseline,"middle")]
  g p >>= append c >>= append t



mkLink :: Link -> IO Element
mkLink link = do
  p <- paper
  line (0,0) (0,0) p


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
                linesE <- mapM mkLink links
                drawGraph (encodeJSON g) (toPtr nodesE) (toPtr linesE)
                          (jsonToJS fromTo)


main = do globalSet "xxx" $ Arr []
          newInput
