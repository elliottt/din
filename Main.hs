module Main where

import Backend.Generic123
import Player

import Control.Concurrent (threadDelay)


createPlayer :: IO Player
createPlayer  = backends =<< initPlayer
  where
  backends p = do
    mpg321 <- mpg321Backend
    ogg123 <- ogg123Backend
    return $ registerBackend ".mp3" mpg321
           $ registerBackend ".ogg" ogg123 p

main :: IO ()
main  = do
  p <- createPlayer
  load "/home/trevor/.local/share/Trash/files/07 - Don't Walk Away.ogg" p
  play p
  threadDelay (20 * 1000 * 1000)
