module Vacuole.UI.Vec where

data Vec = Vec Double Double

instance Num Vec where
    Vec x y + Vec x' y' = Vec (x+x') (y+y')
    negate (Vec x y) = Vec (-x) (-y)
    (*) = undefined
    signum = undefined
    abs = undefined
    fromInteger = undefined

vecScale a (Vec x y) = Vec (a*x) (a*y)

mkVec :: (Int,Int) -> Vec
mkVec (x,y) = Vec (fromIntegral x) (fromIntegral y)

toInts :: Vec -> (Int,Int)
toInts (Vec x y) = (round x, round y)

