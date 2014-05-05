{-# LANGUAGE OverloadedStrings #-}
--module Vacuole.Serve (main) where

import Web.Scotty
import Network.Wai.Middleware.Static
--import Network.Wai.Middleware.RequestLogger
import Data.Text.Lazy.Encoding
import Data.Text.Lazy (pack,unpack)
import Data.Monoid
import Control.Monad.IO.Class

import Vacuole.Log as Log
import Vacuole.View


main = scotty 5555 $ do
         middleware static
--         middleware logStdoutDev
         post "/vac" $ do
                        e <- body
                        let expr = unpack . decodeUtf8 $ e
                        b <- liftIO $ boo expr
                        liftIO $ print (e,b)
                        liftIO $ Log.logExpr "" expr ""
                        text . pack $ show b


