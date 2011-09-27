{-# LANGUAGE DeriveDataTypeable #-}

module Player (
    Player()
  , initPlayer
  , registerBackend

  , load
  , play
  , 
  ) where

import Backend
import Backend.Generic123
import Filesystem

import Control.Concurrent (MVar,newEmptyMVar,tryTakeMVar,withMVar,putMVar)
import Control.Exception (throwIO,Exception)
import Control.Monad (void)
import Data.Typeable (Typeable)
import qualified Data.Map as Map


type BackendMap = Map.Map Extension Backend

data Player = Player
  { playerBackends :: BackendMap
  , playerCurrent  :: MVar Backend
  }

clearCurrent :: Player -> IO ()
clearCurrent p = do
  mb <- tryTakeMVar (playerCurrent p)
  case mb of
    Nothing -> return ()
    Just b  -> backendStop b

setCurrent :: Backend -> Player -> IO ()
setCurrent b p = do
  clearCurrent p
  putMVar (playerCurrent p) b

withCurrent :: Player -> (Backend -> IO ()) -> IO ()
withCurrent p k = do
  let var = playerCurrent p
  mb <- tryTakeMVar var
  case mb of
    Just b  -> k b >> putMVar var b
    Nothing -> return ()


-- Player Interface ------------------------------------------------------------

initPlayer :: IO Player
initPlayer  = do
  current <- newEmptyMVar
  return Player
    { playerBackends = Map.empty
    , playerCurrent  = current
    }

registerBackend :: Extension -> Backend -> Player -> Player
registerBackend ext b p = p
  { playerBackends = Map.insert (normalizeExtension ext) b (playerBackends p)
  }

lookupBackend :: Extension -> Player -> Maybe Backend
lookupBackend ext = Map.lookup (normalizeExtension ext) . playerBackends

backendFor :: Extension -> Player -> IO Backend
backendFor ext p = case lookupBackend ext p of
  Just b  -> return b
  Nothing -> throwIO (NoBackendFor ext)


-- Player Exceptions -----------------------------------------------------------

data PlayerException
  = NoBackendFor Extension
    deriving (Show,Typeable)

instance Exception PlayerException


-- Playback Control ------------------------------------------------------------

type Control = Player -> IO ()

load :: FilePath -> Control
load file p = do
  let ext = takeExtension file
  b <- backendFor ext p
  setCurrent b p
  backendLoad b file
  pause p

play :: Control
play p = withCurrent p backendPlay

stop :: Control
stop p = withCurrent p backendStop

pause :: Control
pause p = withCurrent p backendPause
