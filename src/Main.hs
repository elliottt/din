{-# LANGUAGE OverloadedStrings #-}
module Main where

import Din (runDin,io,logInfo)
import Storage (withStorage)
import Tag
import UI (playerInterface,context)

import Data.Version (showVersion)
import Graphics.Vty.Widgets.All (runUi,newCollection)
import Paths_din (version)


main :: IO ()
main  = withStorage ":memory:" $ runDin $ do

  logInfo ("Starting din-" ++ showVersion version)

  io $ do
    c       <- newCollection
    _mainUi <- playerInterface c
    runUi c context
