{-# LANGUAGE FlexibleInstances #-}
module Vacuole.Snap where
import Haste
import Haste.Prim

data Paper_
data Element_

type Paper = Ptr Paper_
type Element = Ptr Element_

instance Marshal Paper


foreign import cpattern "Snap(%1)" snap :: JSString -> IO Paper
foreign import cpattern "%4.circle(%1,%2,%3)" circle_ :: Int -> Int -> Int -> Paper -> IO Element
foreign import cpattern "%1.g()" g :: Paper -> IO Element
foreign import cpattern "%2.append(%1)" elemAppend :: Element -> Element -> IO Element
foreign import cpattern "%4.text(%1,%2,%3)" text :: Int -> Int -> JSString -> Paper -> IO Element

circle :: (Int,Int) -> Int -> Paper -> IO Element
circle (x,y) r = circle_ x y r
