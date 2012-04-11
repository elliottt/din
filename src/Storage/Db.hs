module Storage.Db where

import Database.SQLite (openConnection,closeConnection,SQLiteHandle,defineTable)
import qualified Control.Exception as E


-- Connection ------------------------------------------------------------------

-- | Run a computation in the context of a connection to the database.
withStorage :: FilePath -> (SQLiteHandle -> IO a) -> IO a
withStorage path = E.bracket (openConnection path) closeConnection
