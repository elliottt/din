module UI where

import Config

import Graphics.Vty
import Graphics.Vty.Widgets.All
import Data.Monoid (mempty)

(%) :: Widget (Box child rest) -> Int -> IO ()
w % p = setBoxChildSizePolicy w (Percentage p)

context :: RenderContext
context  = defaultContext { focusAttr = white `on` blue }

interface :: IO Collection
interface  = do
  c  <- newCollection
  fg <- newFocusGroup

  statusBox <- plainText "status"

  categories <- newStringList mempty ["Library", "Radio"]
  addToFocusGroup fg categories

  artists <- newStringList mempty ["a", "b", "c"]
  addToFocusGroup fg artists

  genres <- newStringList mempty ["awesome", "lame"]
  addToFocusGroup fg genres

  songs <- newStringList mempty ["x", "y", "z"]
  addToFocusGroup fg songs

  agBox <- return artists <++> return genres
  agBox % 50

  songsBox <- return agBox <--> return songs
  songsBox % 25

  mainBox <- return categories <++> return songsBox
  mainBox % 15

  interface <- return statusBox <--> return mainBox
  setBoxChildSizePolicy interface (PerChild (BoxFixed 2) BoxAuto)

  fg `onKeyPressed` \ _ k _ -> case k of
    KASCII 'q' -> shutdownUi >> return True
    _          -> return False

  addToCollection c interface fg

  return c
