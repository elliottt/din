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
  bar    <- newProgressBar white blue
  box    <- hBox text (progressBarWidget bar)
  setBoxChildSizePolicy box (PerChild BoxAuto (BoxFixed 40))

  vCentered box <--> hBorder

playerInterface :: Collection -> IO Choose
playerInterface c = do
  fg <- newFocusGroup

  statusBox <- newStatusBox

  artists <- newStringList mempty ["All artists", "坂本龍", "дни след световната", "c"]
  addToFocusGroup fg artists

  songs <- newStringList mempty ["x", "y", "z"]
  addToFocusGroup fg songs

  mainBox <- return artists <--> hBorder <--> return songs
  mainBox % 25

  interface <- return statusBox <--> return mainBox
  setBoxChildSizePolicy interface (PerChild (BoxFixed 2) BoxAuto)

  fg `onKeyPressed` \ _ k _ -> case k of
    KASCII 'q' -> shutdownUi  >> return True
    KASCII 's' -> focus songs >> return True
    _          -> return False

  addToCollection c interface fg
