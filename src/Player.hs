{-# LANGUAGE DeriveDataTypeable #-}

module Player where

import Backend
import Backend.Generic123
import Filesystem.Utils (Extension,normalizeExtension,takeExtension)

import Control.Concurrent
    (MVar,newMVar,newEmptyMVar,tryTakeMVar,withMVar,putMVar,modifyMVar)
import Control.Exception (throwIO,Exception)
import Control.Monad (void,(<=<))
import Data.List (nub)
import Data.Maybe (catMaybes)
import Data.Typeable (Typeable)
import qualified Data.Map as Map
import qualified Data.Traversable as T


-- Player Handle ---------------------------------------------------------------

data Player = Player
  { playerExtensions :: ExtensionMap
  , playerBackends   :: BackendMap
  , playerCurrent    :: MVar Backend
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


-- Extension Management --------------------------------------------------------

type ExtensionMap = Map.Map Extension BackendName

mapExtension :: Extension -> BackendName -> Player -> Player
mapExtension ext bn p =
  p { playerExtensions = Map.insert ext bn (playerExtensions p) }

mapExtensions :: [(Extension,BackendName)] -> Player -> Player
mapExtensions es p =
  p { playerExtensions = Map.fromList es `Map.union` playerExtensions p }


-- Backend Management ----------------------------------------------------------

data BackendState
  = Running Backend
  | NotRunning (IO Backend)

notRunning :: IO Backend -> IO (MVar BackendState)
notRunning  = newMVar . NotRunning

activate :: BackendState -> IO (BackendState,Backend)
activate s@(Running b)   = return (s,b)
activate (NotRunning mk) = do
  b <- mk
  return (Running b,b)

type BackendMap = Map.Map BackendName (MVar BackendState)

addBackend :: BackendName -> IO Backend -> BackendMap -> IO BackendMap
addBackend n mk m = do
  var <- notRunning mk
  return (Map.insert n var m)

lookupBackend :: BackendName -> BackendMap -> IO Backend
lookupBackend n m = case Map.lookup n m of
  Just var -> modifyMVar var activate
  Nothing  -> throwIO (InvalidBackend n)


-- Player Interface ------------------------------------------------------------

initPlayer :: IO Player
initPlayer  = do
  current  <- newEmptyMVar
  backends <- newMVar Map.empty
  return Player
    { playerExtensions = Map.empty
    , playerBackends   = Map.empty
    , playerCurrent    = current
    }

-- | Register a @Backend@ with a mnemonic name.
registerBackend :: BackendName -> IO Backend -> Player -> IO Player
registerBackend n b p = do
  bs' <- addBackend n b (playerBackends p)
  return p { playerBackends = bs' }

-- | Lookup the name of the @Backend@ associated with this @Extension@.
lookupExtension :: Extension -> Player -> Maybe BackendName
lookupExtension ext = Map.lookup (normalizeExtension ext) . playerExtensions

-- | All currently registered @Backend@s.
registeredBackends :: Player -> [BackendName]
registeredBackends  = Map.keys . playerBackends

-- | All currently running @Backend@s.
runningBackends :: Player -> IO [Backend]
runningBackends  = fmap catMaybes . mapM step . Map.elems . playerBackends
  where
  step var = withMVar var $ \ s -> case s of
    Running b    -> return (Just b)
    NotRunning _ -> return Nothing

-- | Lookup the @Backend@ that will play this @Extension@.
backendFor :: Extension -> Player -> IO (Maybe Backend)
backendFor ext p = T.sequenceA $ do
  bname <- lookupExtension (normalizeExtension ext) p
  return (lookupBackend bname (playerBackends p))

requireBackendFor :: Extension -> Player -> IO Backend
requireBackendFor ext p = do
  mb <- backendFor ext p
  case mb of
    Just b  -> return b
    Nothing -> throwIO (NoBackendFor ext)

-- | Cleanup all @Backend@s associated with the player.
cleanupPlayer :: Player -> IO ()
cleanupPlayer  = mapM_ backendCleanup <=< runningBackends


-- Player Exceptions -----------------------------------------------------------

data PlayerException
  = NoBackendFor Extension
  | InvalidVolume Int
  | InvalidBackend BackendName
    deriving (Show,Typeable)

instance Exception PlayerException


-- Playback --------------------------------------------------------------------

type Control = Player -> IO ()

load :: FilePath -> Control
load path p = do
  b <- requireBackendFor (takeExtension path) p
  setCurrent b p
  backendLoad b path
  pause p

pause :: Control
pause p = withCurrent p backendPause

play :: Control
play p = withCurrent p backendPlay
