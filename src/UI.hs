module UI where

import Config

import Graphics.Vty
import Graphics.Vty.Widgets.All
import Data.Monoid (mempty)

type Choose = IO ()

(%) :: Widget (Box child rest) -> Int -> IO ()
w % p = setBoxChildSizePolicy w (Percentage p)

context :: RenderContext
context  = defaultContext
  { focusAttr = white `on` blue
  }

newStatusBox = do
  text   <- plainText "status"
  bar    <- newProgressBar blue cyan
  box    <- hBox text (progressBarWidget bar)
  setBoxChildSizePolicy box (PerChild BoxAuto (BoxFixed 40))

  setProgress bar 50

  vCentered box

playerInterface :: Collection -> IO Choose
playerInterface c = do
  fg <- newFocusGroup

  statusBox <- newStatusBox

  songs <- newStringList mempty ["x", "y", "z"]
  addToFocusGroup fg songs

  interface <- return statusBox <--> return songs
  setBoxChildSizePolicy interface (PerChild (BoxFixed 2) BoxAuto)

  songs `onKeyPressed` \ _ k _ -> case k of
    KASCII 'k' -> scrollUp songs   >> return True
    KASCII 'j' -> scrollDown songs >> return True
    _          -> return False

  fg `onKeyPressed` \ _ k _ -> case k of
    KASCII 'q' -> shutdownUi  >> return True
    KASCII 's' -> focus songs >> return True
    _          -> return False

  addToCollection c interface fg
