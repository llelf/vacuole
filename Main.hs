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
  textRequest_ POST "/vac" [("expr",v)] $ \res ->
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


linkEnds (Single link) = link
linkEnds (Multi link _) = link


showGraph g = do
  let (nodes,links0) = g :: GraphView
      links = linksGatherMulti links0

  print links

  let nodesMap = Map.fromList $ zip [0..] nodes
      linksMap = Map.fromList $ zip [0..] links
                 
  let fromTo = Arr $
               map (\(Link s t) -> Arr $ map (Num . fromIntegral) [s,t]) $ map linkEnds $ links

  arrow <- arrowDef

  nodesE <- mapM mkNode nodes
  zoo <- mapM (mkLink nodesMap arrow) links
  let linkElems = Map.fromList $ zip [0..] zoo

  print linkElems

  draw nodesE linkElems
  drawGraph (toPtr nodesE) (jsonToJS fromTo)
            (toPtr $ tickN nodesMap)
            (toPtr $ tickL linksMap linkElems)




main = do initD3 (toPtr linkStrength)
          initTerm (toPtr newInput)



