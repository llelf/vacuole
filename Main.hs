module Main where

import Haste
import Haste.Prim
import Vacuole.Snap

jsMain' :: IO ()
jsMain' = ffi "alert('hi')"

jsMain :: IO ()
jsMain = ffi "newInput()"

globalSet :: String -> Int -> IO ()
globalSet var = ffi $ "(function(x){" ++ var ++ "=x; return {}})"



paper :: IO (Ptr Paper)
paper = ffi "Paper"

-- paperId :: Ptr Paper -> IO String
-- paperId = ffi "(function(p) {return p.id})"

-- circle :: Int -> Int -> Int -> IO ()
-- circle = ffi "(function (x,y,r){return Paper.circle(x,y,r)})"


main = do globalSet "baz" 2
          putStrLn "ok."
          p <- paper
          c <- circle 50 50 10 p
          t <- text 50 50 (toJSString "text") p
          g p >>= elemAppend c >>= elemAppend t



