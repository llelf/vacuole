{-# LANGUAGE StandaloneDeriving, DeriveDataTypeable, TupleSections #-}
module Vacuole.Interp where

import qualified Language.Haskell.Interpreter as H
import GHC.Vacuum
import Data.IntMap.Strict as IM (elems,mapWithKey,IntMap)
import Data.Typeable
import System.IO.Unsafe
import Data.Char
import Data.Either
import Control.Monad

type Vac = IntMap HNode

deriving instance Typeable HNode


vacuumise s = do res <- H.runInterpreter interp
                 return $ case res of
                    Left err -> Left $ show err
                    Right s -> Right s
    where
      interp :: H.Interpreter Vac
      interp = do
        H.setImports ["GHC.Vacuum", "Data.IntMap.Strict"]
        H.interpret ("vacuum (" ++ s ++ ")") (H.as :: Vac)



graph s = do Right v <- vacuumise s
             return (nodes v, links v)


e = vacuum (1::Integer)
a=A

data Con = A|B|C

data Colour = Green | Blue | Red
              deriving Show

data Node = Node {
      size::Int, colour::Colour, name::String, desc::String
} deriving Show

node n t = Node {size=10, colour=Blue, name=n, desc=t}

toJS nd @HNode {nodeLits=lits, nodeInfo=info}
    | n=="S#" || n=="I#" = node (show $ head $ lits) "int"
    | n=="C#" = node (show $ chr $ fromIntegral $ head $ lits) "char"
    | n==":"  = node "(:)" ""
    | n=="[]"  = node "[]" ""
    where n = nodeName nd

-- data E = A{a::Int} | B{b::Int}
--          deriving Show


nodes = map toJS . elems

links = concat . elems . mapWithKey (\k (HNode {nodePtrs=ls}) -> map (k,) ls)
