{-# LANGUAGE OverloadedStrings #-}
module Main where

import Config (parseConfig,Config(..),emptyConfig)
import Din (Din,runDin,io,logInfo)
import Din.Types (Env(..))
import Options (parseOptions,Options(..))
import Storage (withStorage,initDb)
import Tag
import UI (playerInterface,context)

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
  withEnv opts cfg (runDin dinMain)

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

withEnv :: Options -> Config -> (Env -> IO a) -> IO a
withEnv opts cfg k = do
  isFresh <- doesFileExist (cfgDbPath cfg)
  withStorage (cfgDbPath cfg) $ \ h -> k Env
    { envDbHandle = h
    , envLogLevel = optLogLevel opts
    , envDbFresh  = isFresh
    }

dinMain :: Din ()
dinMain  = do
  logInfo ("Starting din-" ++ showVersion version)

  initDb

  -- run the ui
  io $ do
    c       <- newCollection
    _mainUi <- playerInterface c
    runUi c context
