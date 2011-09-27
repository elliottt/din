{-# LANGUAGE DeriveDataTypeable #-}

module Backend (
    Backend(..)
  , BackendException(..)
  ) where

import Control.Exception (Exception)
import Data.Typeable (Typeable)


-- | An integer between 0 and 100.
type Volume = Int

data Backend = Backend
  { backendLoad      :: FilePath -> IO ()
  , backendPlay      :: IO ()
  , backendPause     :: IO ()
  , backendStop      :: IO ()
  , backendCleanup   :: IO ()
  , backendStatus    :: IO String
  , backendSetVolume :: Volume -> IO ()
  }

data BackendException
  = BackendCrashed String
    deriving (Show,Typeable)

instance Exception BackendException

