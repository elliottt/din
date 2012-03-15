module Main where

import UI
import Tag

import Graphics.Vty.Widgets.All (runUi,newCollection)


main :: IO ()
main  = do
  c       <- newCollection
  _mainUi <- playerInterface c
  runUi c context
