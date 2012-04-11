module Din.Types where

import Database.SQLite (SQLiteHandle)


-- Din Environment -------------------------------------------------------------

data Env = Env
  { envDbHandle :: SQLiteHandle
  , envLogLevel :: LogLevel
  }


-- Log Levels ------------------------------------------------------------------

data LogLevel = LogError | LogWarn | LogInfo | LogDebug
    deriving (Show,Eq,Ord,Enum,Bounded)
