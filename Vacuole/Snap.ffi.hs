{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Vacuole.Snap where
import Haste
import Haste.Prim
import Haste.JSON
import Data.Char
import Control.Monad
import Haste.Foreign

data Paper_
data Element_

type Paper = Ptr Paper_
type Element = Ptr Element_

instance Marshal Paper

data Attr = AlignmentBaseline|Class|Fill
          |MarkerBegin|MarkerMid|MarkerEnd
          |Scale|Stroke|TextAnchor|Transform
            deriving (Show,Read)

camelToLispCase []           = []
camelToLispCase (a:b:rs) | isLower a && isUpper b
                             = (a : '-' : toLower b : camelToLispCase rs)
camelToLispCase (a:rs)       = (toLower a : camelToLispCase rs)


foreign import cpattern "Snap(%1)" snap :: JSString -> IO Paper
foreign import cpattern "%4.circle(%1,%2,%3)" circle_ :: Int -> Int -> Int -> Paper -> IO Element
foreign import cpattern "%5.line(%1,%2,%3,%4)" line_ :: Int->Int->Int->Int->Paper->IO Element
foreign import cpattern "%2.path(%1)" path_ :: JSString->Paper->IO Element
foreign import cpattern "%1.g()" g :: Paper -> IO Element
foreign import cpattern "%2.append(%1)" append :: Element -> Element -> IO Element
foreign import cpattern "%4.text(%1,%2,%3)" text_ :: Int -> Int -> JSString -> Paper -> IO Element
foreign import cpattern "(function(){var e={};e[%1]=%2;return %3.attr(e)})()" setAttr_ :: JSString -> JSString -> Element -> IO Element
foreign import cpattern "(function(){var e={};e[%1]=E(E(%2)[1]);return %3.attr(e)})()" setAttrPtr_ :: JSString -> Ptr a -> Element -> IO Element
foreign import cpattern "%7.marker(%1,%2,%3,%4,%5,%6)" marker_ :: Int->Int->Int->Int->Int->Int->Element->IO Element

circle :: (Int,Int) -> Int -> Paper -> IO Element
circle (x,y) r = circle_ x y r

text :: (Int,Int) -> String -> Paper -> IO Element
text (x,y) t = text_ x y (toJSString t)

line :: (Int,Int) -> (Int,Int) -> Paper -> IO Element
line (x,y) (x',y') = line_ x y x' y'

path :: String -> Paper -> IO Element
path p = path_ (toJSString p)

marker :: (Int,Int) -> (Int,Int) -> (Int,Int) -> Element -> IO Element
marker (x,y) (w,h) (xref,yref) = marker_ x y w h xref yref


foreign import cpattern "JSON.parse(%1)" jsParseJSON :: JSString -> JSAny


translate :: Int -> Int -> JSString
translate x y = toJSString $ "translate("++show x++","++show y++")"

scale :: Double -> JSString
scale x = toJSString $ "scale("++show x++")"


jsonToJS :: JSON -> JSAny
jsonToJS = jsParseJSON . encodeJSON


attrToJSStr = toJSStr . camelToLispCase . show

setAttr :: (Attr,JSString) -> Element -> IO Element
setAttr (k,v) = setAttr_ (attrToJSStr k) v

setAttrs :: [(Attr,JSString)] -> Element -> IO Element
setAttrs as e = foldM (\e kv -> setAttr kv e) e as

setAttrPtr :: (Attr,Ptr a) -> Element -> IO Element
setAttrPtr (k,v) = setAttrPtr_ (attrToJSStr k) v
