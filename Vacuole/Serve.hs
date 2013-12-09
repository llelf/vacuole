{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty
import Network.Wai.Middleware.Static
import Data.Text.Lazy.Encoding
import Data.Text.Lazy (unpack)
import Data.Monoid
import Control.Monad.IO.Class

import Vacuole.View


main = scotty 5555 $ do
         middleware static
         post "/vac" $ do
                        e <- body
                        let t = decodeUtf8 e
                        b <- liftIO $ boo $ unpack t
                        text $ "fuck you, " <> t <> ", " <> decodeUtf8 b





