{-# LANGUAGE OverloadedStrings #-}
module Vacuole.UI.Draw.Nodes where

import Vacuole.Snap
import Vacuole.View.Types
import Control.Monad


mkNode :: Node -> Paper -> IO Element
mkNode (Node (Vanilla s) _) = vanillaNode s
mkNode (Node EmptyList _)  = vanillaNode "[]"
mkNode (Node ArrWords _) = memNode
mkNode (Node ArrPtrs _) = memNode
mkNode (Node Cons _) = consNode
mkNode (Node Fun _)  = vanillaNode "λ"
mkNode (Node Nowhere _) = nowhereNode


nowhereNode = genericNode 0 "nowhere" "…"

consNode = genericNode 15 "cons" "(:)"


memNode p = do
  cs <- forM [3,2..0] $ \x ->
          circle (x*3,x*3) 20 p >>= setAttrs [(Class,"mem")]
  g <- g p
  foldM append g cs

vanillaNode = genericNode 20 "vanilla"

genericNode size svgcls str p = do
  c <- circle (0,0) size p >>= setAttrs [(Class, svgcls)]
  t <- text (0,0) str p >>= setAttrs [(TextAnchor,"middle"),
                                      (AlignmentBaseline,"middle")]
  g p >>= flip append c >>= flip append t

