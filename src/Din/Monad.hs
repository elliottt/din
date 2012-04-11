{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Din.Monad (
    -- * Din Monad
    Din
  , runDin

    -- * Primitive Operations
  , io
  , dbHandle

    -- * Message Logging
  , LogLevel(..)
  , logDebug
  , logInfo
  , logWarn
  , logError
  ) where

import Din.Types (LogLevel(..),Env(..))
import Options (withOptionsEnv)

import Control.Applicative (Applicative)
import Control.Monad (when)
import Database.SQLite (SQLiteHandle)
import MonadLib (ReaderT,ReaderM(..),BaseM(..),runM)


-- Din Monad -------------------------------------------------------------------

newtype Din a = Din
  { unDin :: ReaderT Env IO a
  } deriving (Functor,Applicative,Monad)

runDin :: Din a -> Env -> IO a
runDin (Din m) = runM m

-- | Lift an @IO@ action into the @Din@ monad.
io :: IO a -> Din a
io m = Din (inBase m)

-- | Retrieve the database handle.
dbHandle :: Din SQLiteHandle
dbHandle  = envDbHandle `fmap` Din ask

-- | Return the current @LogLevel@.
logLevel :: Din LogLevel
logLevel  = envLogLevel `fmap` Din ask


-- Logging ---------------------------------------------------------------------

logLevelTag :: LogLevel -> Char
logLevelTag lev = case lev of
  LogError -> 'E'
  LogWarn  -> 'W'
  LogInfo  -> 'I'
  LogDebug -> 'D'

logMessage :: LogLevel -> String -> Din ()
logMessage lev str = do
  threshold <- logLevel
  when (lev <= threshold)
    (io (putStrLn (concat [ "[", [logLevelTag lev], "] ", str ])))

logDebug, logInfo, logWarn, logError :: String -> Din ()
logInfo  = logMessage LogInfo
logDebug = logMessage LogDebug
logWarn  = logMessage LogWarn
logError = logMessage LogError
