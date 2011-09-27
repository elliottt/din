{-# LANGUAGE DeriveDataTypeable #-}

module Backend (
    Backend(..)
  , BackendException(..)
  ) where

import Control.Exception (Exception)
import Data.Typeable (Typeable)


data Backend = Backend
  { backendLoad    :: FilePath -> IO ()
  , backendPlay    :: IO ()
  , backendPause   :: IO ()
  , backendStop    :: IO ()
  , backendCleanup :: IO ()
  , backendStatus  :: IO String
  }

data BackendException
  = BackendCrashed String
    deriving (Show,Typeable)

instance Exception BackendException

