{-# LANGUAGE FlexibleInstances #-}
module Vacuole.Snap where
import Haste
import Haste.Prim

data Paper
data Element

type JSPaper = Ptr Paper
type JSElement = Ptr Element

instance Marshal (Ptr Paper)


foreign import cpattern "Snap(%1)" snap :: JSString -> IO (Ptr Paper)
foreign import cpattern "%4.circle(%1,%2,%3)" circle :: Int -> Int -> Int -> JSPaper -> IO JSElement
foreign import cpattern "%1.g()" g :: JSPaper -> IO JSElement
foreign import cpattern "%2.append(%1)" elemAppend :: JSElement -> JSElement -> IO JSElement
foreign import cpattern "%4.text(%1,%2,%3)" text :: Int -> Int -> JSString -> JSPaper -> IO JSElement

