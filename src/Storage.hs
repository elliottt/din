module Storage (
    -- * Storage
    withStorage

    -- * Table Definitions
  , songTable
  ) where

import Database.SQL.Types
    (SQLTable,SQLType(..),Table(..),Column(..),IntType(..),Clause(..)
    ,SQLType(..))
import Database.SQLite (openConnection,closeConnection,SQLiteHandle,defineTable)
import qualified Control.Exception as E


-- Connection ------------------------------------------------------------------

-- | Run a computation in the context of a connection to the database.
withStorage :: FilePath -> (SQLiteHandle -> IO a) -> IO a
withStorage path = E.bracket (openConnection path) closeConnection


-- Schema ----------------------------------------------------------------------

type SQLColumn = Column SQLType

idCol :: SQLColumn
idCol  = Column
  { colName    = "id"
  , colType    = SQLInt NORMAL True False
  , colClauses = [PrimaryKey True, Unique, IsNullable False]
  }

intCol :: String -> SQLColumn
intCol name = Column
  { colName    = name
  , colType    = SQLInt NORMAL False False
  , colClauses = [IsNullable True]
  }

varCharCol :: String -> Int -> SQLColumn
varCharCol name len = Column
  { colName    = name
  , colType    = SQLVarChar len
  , colClauses = [IsNullable True]
  }

songTable :: SQLTable
songTable  = Table
  { tabName        = "songs"
  , tabColumns     =
    [ idCol
    , (varCharCol "uri" 2048) { colClauses = [IsNullable False] }
    ]
  , tabConstraints = []
  }

infoTable :: SQLTable
infoTable  = Table
  { tabName        = "info"
  , tabColumns     =
    [ idCol
    , (intCol "type") { colClauses = [IsNullable False] }
    , varCharCol "string" 1024
    , intCol "number"
    ]
  , tabConstraints = []
  }
