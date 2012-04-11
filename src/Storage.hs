module Storage (
    module Storage
  , module Exports
  ) where

import Din
import Storage.Db as Exports
import Storage.Schema (dinSchema)

import Control.Monad (when)
import Database.SQL (SQLTable,tabName)
import Database.SQLite (defineTable)


-- Initialization --------------------------------------------------------------

-- | Create tables in the database, if it's freshly created.
initDb :: Din ()
initDb  = do
  isFresh <- freshDb
  when isFresh (mapM_ createTable dinSchema)

-- | Given a specification, create a table in the database.
createTable :: SQLTable -> Din ()
createTable def = do
  logInfo ("Creating table " ++ tabName def)
  h  <- dbHandle
  mb <- io (defineTable h def)
  case mb of
    Just _  -> return ()
    Nothing -> logError ("Failed when creating table: " ++ tabName def)
