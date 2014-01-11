{-# LANGUAGE StandaloneDeriving, DeriveDataTypeable, TupleSections #-}
module Vacuole.Interp (module GHC.Vacuum,
                       Vacuum, vacuumise
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

type Vacuum = IntMap HNode

deriving instance Typeable HNode



imports = [
 "GHC.Vacuum",
 "Data.IntMap.Strict",
 "Vacuole.Prelude",
 "Prelude",
 "Data.List"
 ]


vacuumise :: String -> IO (Either String Vacuum)
vacuumise s = do res <- H.runInterpreter interp
                 return $ case res of
                    Left err -> Left $ show err
                    Right s -> Right s
    where
      interp :: H.Interpreter Vacuum
      interp = do
        H.loadModules ["Vacuole.Prelude"]
        H.setImports imports
        H.interpret ("vacuumTo 10 (" ++ s ++ ")") (H.as :: Vacuum)


