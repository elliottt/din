module Main where

import Backend.Generic123
import Player

import Control.Concurrent (threadDelay)
import Control.Exception (bracket)


createPlayer :: IO Player
createPlayer  = exts `fmap` create
  where
  create = registerBackend "mp3" mpg321Backend
       =<< registerBackend "ogg" ogg123Backend
       =<< initPlayer

  exts   = mapExtensions [(".mp3", "mp3"), (".ogg", "ogg")]

main :: IO ()
main  = bracket createPlayer cleanupPlayer $ \ p -> do
  load "/home/trevor/.local/share/Trash/files/07 - Don't Walk Away.ogg" p
  play p
  threadDelay (5 * 1000 * 1000)
