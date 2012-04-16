module Din.Types where

import Database.SQLite (SQLiteHandle)
import Player (Player)


-- Din Environment -------------------------------------------------------------

data Env = Env
  { envDbHandle :: SQLiteHandle
  , envLogLevel :: LogLevel
  , envDbFresh  :: Bool
  , envPlayer   :: Player
  }


-- Log Levels ------------------------------------------------------------------

data LogLevel = LogError | LogWarn | LogInfo | LogDebug
    deriving (Show,Eq,Ord,Enum,Bounded)
