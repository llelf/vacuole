{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty
import Network.Wai.Middleware.Static
import Data.Text.Lazy.Encoding

import Vacuole.Interp


main = scotty 5555 $ do
         middleware static
         post "/vac" $ do
                        e <- body
                        let t = decodeUtf8 e
                        text t




