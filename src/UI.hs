module UI where

import Config
import UI.PlaybackStatus

import Graphics.Vty
import Graphics.Vty.Widgets.All
import Data.Monoid (mempty)

type Choose = IO ()

context :: RenderContext
context  = defaultContext
  { focusAttr = white `on` blue
  }

-- | The status box consists of a single progress bar widget, whose text
-- contents are the status of the currently playing song.
newStatusBox = do
  (play,bar) <- newPlaybackStatus (white `on` blue) (black `on` cyan)

  setPlaybackProgress play 50
  setPlaybackArtist play "the artist name"
  setPlaybackTitle play "the title name"

  return bar

playerInterface :: Collection -> IO Choose
playerInterface c = do
  fg <- newFocusGroup

  statusBox <- newStatusBox

  songs <- newStringList mempty ["x", "y", "z"]
  addToFocusGroup fg songs

  interface <- return statusBox <--> return songs
  setBoxChildSizePolicy interface (PerChild (BoxFixed 1) BoxAuto)

  songs `onKeyPressed` \ _ k _ -> case k of
    KASCII 'k' -> scrollUp songs   >> return True
    KASCII 'j' -> scrollDown songs >> return True
    _          -> return False

  fg `onKeyPressed` \ _ k _ -> case k of
    KASCII 'q' -> shutdownUi  >> return True
    KASCII 's' -> focus songs >> return True
    _          -> return False

  addToCollection c interface fg
