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
    parseJSON json = Node 10 Red "?" "?"

instance FromJSON Link where
    parseJSON json = Link 0 0


jsMain' :: IO ()
jsMain' = ffi "alert('hi')"

jsMain :: IO ()
jsMain = ffi "newInput()"

globalSet :: String -> Int -> IO ()
globalSet var = ffi $ "(function(x){" ++ var ++ "=x; return {}})"


paper :: IO Paper
paper = ffi "Paper"


inputValue :: IO JSString
inputValue = ffi "d3.select('textarea').property('value')"

canvasClear :: IO ()
canvasClear = ffi "canvasClear()"

foreign import ccall drawGraph
    :: JSString -> Ptr [Element] -> IO ()


mkNode :: Int -> IO Element
mkNode n = do
  p <- paper
  text (0,0) (show n) p

newInput = do v <- inputValue
              nodes <- mapM mkNode [1..3]
              print v
              jsonRequest_ POST "/vac"
                               [("expr",v)] $ \d -> do
                print d
                canvasClear
                let Just g = d
                drawGraph (encodeJSON g) (toPtr nodes)


main = newInput

