{-# LANGUAGE OverloadedStrings #-}
module Main where

import Backend.Generic123 (mpg321Backend,ogg123Backend)
import Config (parseConfig,Config(..),emptyConfig)
import Din (Din,runDin,io,logInfo,forkDin)
import Din.Types (Env(..))
import Options (parseOptions,Options(..))
import Player (Player,initPlayer,registerBackend,mapExtension)
import Storage (withStorage,initDb)
import Tag
import UI (playerInterface,context)
import Watch (watcher)

import Data.Version (showVersion)
import Graphics.Vty.Widgets.All (runUi,newCollection)
import Paths_din (version)
import System.Environment (getArgs)
import System.Directory (doesFileExist,createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import qualified Data.Text.Lazy.IO as L


main :: IO ()
main  = do
  args      <- getArgs
  opts      <- parseOptions args
  let dbPath = takeDirectory (optConfigFile opts)
  createDirectoryIfMissing True dbPath
  cfg       <- loadConfig opts
  withEnv opts cfg (runDin (dinMain cfg))

loadConfig :: Options -> IO Config
loadConfig opts = do
  let dbPath = takeDirectory (optConfigFile opts)
  cfgExists <- doesFileExist (optConfigFile opts)
  if not cfgExists
    then return (emptyConfig (takeDirectory (optConfigFile opts)))
    else do
      text <- L.readFile (optConfigFile opts)
      case parseConfig dbPath text of
        Right cfg -> return cfg
        Left err  -> fail err

setupPlayer :: IO Player
setupPlayer  = do
  p <- initPlayer

  registerBackend "mpg321" mpg321Backend p
  registerBackend "ogg123" ogg123Backend p

  return $ mapExtension ".mp3" "mpg321"
         $ mapExtension ".ogg" "ogg123" p

checkForDb :: FilePath -> IO Bool
checkForDb path = case path of
  ":memory:" -> return True
  _          -> doesFileExist path

withEnv :: Options -> Config -> (Env -> IO a) -> IO a
withEnv opts cfg k = do
  isFresh <- checkForDb (cfgDbPath cfg)
  p       <- setupPlayer
  withStorage (cfgDbPath cfg) $ \ h -> k Env
    { envDbHandle = h
    , envLogLevel = optLogLevel opts
    , envDbFresh  = isFresh
    , envPlayer   = p
    }

dinMain :: Config -> Din ()
dinMain cfg = do
  logInfo ("Starting din-" ++ showVersion version)

  initDb

  _ <- forkDin (watcher (cfgWatchPaths cfg))

  -- run the ui
  io $ do
    c       <- newCollection
    _mainUi <- playerInterface c
    runUi c context
