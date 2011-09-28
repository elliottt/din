{-# LANGUAGE DeriveDataTypeable #-}

module Backend (
    Backend(..)
  , BackendName
  , BackendException(..)
  ) where

import Control.Exception (Exception)
import Data.Function (on)
import Data.Typeable (Typeable)


-- | An integer between 0 and 100.
type Volume = Int

type BackendName = String

data Backend = Backend
  { backendName      :: BackendName
  , backendLoad      :: FilePath -> IO ()
  , backendPlay      :: IO ()
  , backendPause     :: IO ()
  , backendStop      :: IO ()
  , backendCleanup   :: IO ()
  , backendStatus    :: IO String
  , backendSetVolume :: Volume -> IO ()
  }

instance Eq Backend where
  (==) = (==) `on` backendName

data BackendException
  = BackendCrashed String
    deriving (Show,Typeable)

instance Exception BackendException

