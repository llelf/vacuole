{-# LANGUAGE OverloadedStrings #-}
module Main where

import Haste
import Haste.Prim
import Haste.Ajax
import Haste.JSON
import Control.Applicative
import qualified Data.IntMap as Map

import Vacuole.View.Types
import Vacuole.Snap
import Vacuole.UI.Multi
import Vacuole.UI.Draw



foreign import ccall initTerm :: Ptr (JSString -> IO Bool) -> IO ()
foreign import ccall initD3 :: Ptr (Int -> Double) -> IO ()

defaultInput = "[1..3]"


newInput :: JSString -> IO Bool
newInput v = do
  print v
  textRequestWithData_ POST "/vac" [] v $ \res ->
      do
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



main = do initD3 (toPtr linkStrength)
          initTerm (toPtr newInput)



