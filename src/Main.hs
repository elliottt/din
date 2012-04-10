{-# LANGUAGE OverloadedStrings #-}
module Main where

import Din (runDin,io)
import Storage (withStorage)
import Tag
import UI (playerInterface,context)

import Graphics.Vty.Widgets.All (runUi,newCollection)


main :: IO ()
main  = withStorage ":memory:" $ runDin $ do

  io $ do
    c       <- newCollection
    _mainUi <- playerInterface c
    runUi c context
