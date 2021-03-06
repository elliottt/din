module Storage.Schema (
    -- * Schema
    dinSchema

    -- * Table Definitions
  , songTable
  , infoTable
  ) where

import Database.SQL.Types
    (SQLTable,SQLType(..),Table(..),Column(..),IntType(..),Clause(..)
    ,SQLType(..))


-- Schema ----------------------------------------------------------------------

dinSchema :: [SQLTable]
dinSchema  =
  [ songTable
  , infoTable
  ]


-- Tables-----------------------------------------------------------------------

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
