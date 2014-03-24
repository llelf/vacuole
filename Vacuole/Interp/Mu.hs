
module Vacuole.Interp.Mu where

import qualified Vacuole.Interp as I
import qualified Data.IntMap as M

import GHC.Vacuum
import GHC.Vacuum.ClosureType



vacuumise :: String -> IO (Either String I.Vacuum)
vacuumise _ = return $ Right o
    where o = (M.fromList [(0,HNode {nodePtrs = [], nodeLits = [1], nodeInfo = ConInfo {itabPkg = "integer-gmp", itabMod = "GHC.Integer.Type", itabCon = "S#", itabPtrs = 0, itabLits = 1, itabType = CONSTR_0_1, itabSrtLen = 0, itabCode = [72,255,195,255,101,0,24,0,0,0,102,15,31,68,0,0]}})])













