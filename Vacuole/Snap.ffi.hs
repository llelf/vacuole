{-# LANGUAGE FlexibleInstances #-}
module Vacuole.Snap where
import Haste
import Haste.Prim
import Haste.JSON
import Data.Char

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

instance Marshal Attr where
    unpack = unpack . toJSStr . show
    pack = read . fromJSStr . pack

foreign import cpattern "Snap(%1)" snap :: JSString -> IO Paper
foreign import cpattern "%4.circle(%1,%2,%3)" circle_ :: Int -> Int -> Int -> Paper -> IO Element
foreign import cpattern "%1.g()" g :: Paper -> IO Element
foreign import cpattern "%2.append(%1)" append :: Element -> Element -> IO Element
foreign import cpattern "%4.text(%1,%2,%3)" text_ :: Int -> Int -> JSString -> Paper -> IO Element
foreign import cpattern "%1.attr(%2)" setAttrs_ :: Element -> JSAny -> IO Element

circle :: (Int,Int) -> Int -> Paper -> IO Element
circle (x,y) r = circle_ x y r

text :: (Int,Int) -> String -> Paper -> IO Element
text (x,y) t = text_ x y (toJSString t)


foreign import cpattern "JSON.parse(%1)" jsParseJSON :: JSString -> JSAny


jsonToJS :: JSON -> JSAny
jsonToJS = jsParseJSON . encodeJSON


setAttrs :: Element -> [(Attr,String)] -> IO Element
setAttrs e as = do
  setAttrs_ e (jsonToJS ajson)
  return e
    where ajson = Dict $ map (\(k,v) -> (toJSStr $ camelToLispCase $ show k, Str $ toJSStr v)) as
