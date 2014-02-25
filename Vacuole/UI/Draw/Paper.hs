module Vacuole.UI.Draw.Paper (paper) where

import Haste (ffi)
import Vacuole.Snap


-- XXX global. Kill
paper :: IO Paper
paper = ffi "Paper"

