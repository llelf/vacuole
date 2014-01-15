module Vacuole.UI.Vec (Vec,scale,mkVec,coords,rotL,rotR)
    where

data Vec = Vec Double Double

instance Num Vec where
    Vec x y + Vec x' y' = Vec (x+x') (y+y')
    negate (Vec x y) = Vec (-x) (-y)
    (*) = undefined
    signum = undefined
    abs = undefined
    fromInteger = undefined

scale a (Vec x y) = Vec (a*x) (a*y)

mkVec :: (Int,Int) -> Vec
mkVec (x,y) = Vec (fromIntegral x) (fromIntegral y)

coords :: Vec -> (Int,Int)
coords (Vec x y) = (round x, round y)

rotL (Vec x y) = Vec (-y) x
rotR (Vec x y) = Vec y (-x)

