module Main where

import UI

import Graphics.Vty
import Graphics.Vty.Widgets.All (runUi)


main :: IO ()
main  = do
  c <- interface
  runUi c context
