{-# LANGUAGE OverloadedStrings #-}
module Main where

import Din (Din,runDin,io,logInfo)
import Options (parseOptions,withOptionsEnv)
import Storage (withStorage)
import Tag
import UI (playerInterface,context)

import Data.Version (showVersion)
import Graphics.Vty.Widgets.All (runUi,newCollection)
import Paths_din (version)
import System.Environment (getArgs)


main :: IO ()
main  = do
  args    <- getArgs
  options <- parseOptions args
  withOptionsEnv options (runDin dinMain)

dinMain :: Din ()
dinMain  = do
  logInfo ("Starting din-" ++ showVersion version)
  -- run the ui
  io $ do
    c       <- newCollection
    _mainUi <- playerInterface c
    runUi c context
