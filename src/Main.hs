{-# LANGUAGE OverloadedStrings #-}
module Main where

import Storage.Query
import Storage.Schema (migrateAll)
import UI (playerInterface,context)
import Tag

import Control.Monad.Trans (liftIO)
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Graphics.Vty.Widgets.All (runUi,newCollection)


main :: IO ()
main  = withSqliteConn ":memory:" $ runSqlConn $ do
  runMigration migrateAll
  liftIO $ do
    c       <- newCollection
    _mainUi <- playerInterface c
    runUi c context
