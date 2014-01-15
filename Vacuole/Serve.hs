{-# LANGUAGE OverloadedStrings #-}
--module Vacuole.Serve (main) where

import Web.Scotty
import Network.Wai.Middleware.Static
import Data.Text.Lazy.Encoding
import Data.Text.Lazy (pack,unpack)
import Data.Monoid
import Control.Monad.IO.Class

import Vacuole.View


main = scotty 5555 $ do
         middleware static
         post "/vac" $ do
                        e <- body
                        b <- liftIO . boo . unpack . decodeUtf8 $ e
                        liftIO $ print (e,b)
                        text . pack $ show b






