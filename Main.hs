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

-- paperId :: Ptr Paper -> IO String
-- paperId = ffi "(function(p) {return p.id})"

-- circle :: Int -> Int -> Int -> IO ()
-- circle = ffi "(function (x,y,r){return Paper.circle(x,y,r)})"


node = decodeJSON (toJSString "{'a':2}")

inputValue :: IO JSString
inputValue = ffi "d3.select('textarea').property('value')"

canvasClear :: IO ()
canvasClear = ffi "canvasClear()"

drawGraph :: JSString -> IO ()
drawGraph = ffi "(function (g) { return drawGraph(g) })"

newInput = do v <- inputValue
              print v
              jsonRequest_ POST "/vac"
                               [("expr",v)] $ \d -> do
                print d
                canvasClear
                let Just g = d
                drawGraph $ encodeJSON g


main = do newInput







