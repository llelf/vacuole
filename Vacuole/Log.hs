{-# LANGUAGE OverloadedStrings #-}
module Vacuole.Log (logExpr) where

import Database.SQLite.Simple
import Database.SQLite.Simple.FromField
import Data.Time.Clock

logExpr :: String -> String -> String -> IO ()
logExpr ip expr status =
    withConnection "log.db" $ \conn -> do
      time <- getCurrentTime
      execute conn "insert into log (time,ip,expr,status) values (?,?,?,?)"
                  (time,ip,expr,status)


