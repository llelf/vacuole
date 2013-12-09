{-# LANGUAGE StandaloneDeriving, DeriveDataTypeable, TupleSections #-}
module Vacuole.Interp (module GHC.Vacuum,
                       vacuumise
                      )
    where

import qualified Language.Haskell.Interpreter as H
import GHC.Vacuum
import Data.IntMap.Strict as IM (elems,mapWithKey,IntMap)
import Data.Typeable
import System.IO.Unsafe
import Data.Char
import Data.Either
import Control.Monad
import Data.Aeson

import Vacuole.Prelude

type Vac = IntMap HNode

deriving instance Typeable HNode


vacuumise s = do res <- H.runInterpreter interp
                 return $ case res of
                    Left err -> Left $ show err
                    Right s -> Right s
    where
      interp :: H.Interpreter Vac
      interp = do
        H.loadModules ["Vacuole.Prelude"]
        H.setImports ["GHC.Vacuum", "Data.IntMap.Strict", "Vacuole.Prelude", "Prelude"]
        H.interpret ("vacuum (" ++ s ++ ")") (H.as :: Vac)




e = vacuum (1::Integer)

