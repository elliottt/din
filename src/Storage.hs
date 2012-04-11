module Storage (
    module Storage
  , module Exports
  ) where

import Din
import Storage.Db as Exports
import Storage.Schema (songTable,infoTable)

import Database.SQL (SQLTable,tabName)
import Database.SQLite (defineTable)


-- Initialization --------------------------------------------------------------

initDb :: Din ()
initDb  = do
  createTable songTable
  createTable infoTable

createTable :: SQLTable -> Din ()
createTable def = do
  h  <- dbHandle
  mb <- io (defineTable h def)
  case mb of
    Just _  -> return ()
    Nothing -> logError ("Failed when creating table: " ++ tabName def)
