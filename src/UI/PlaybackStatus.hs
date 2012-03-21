module UI.PlaybackStatus where

import Graphics.Vty
import Graphics.Vty.Widgets.All
import Data.IORef (IORef,newIORef,readIORef,writeIORef)


data PlaybackStatus = PlaybackStatus
  { playbackWidget :: Widget ProgressBar
  , playbackArtist :: IORef String
  , playbackTitle  :: IORef String
  }

newPlaybackStatus :: Attr -> Attr -> IO (PlaybackStatus, Widget ProgressBar)
newPlaybackStatus comp incomp = do
  bar  <- newProgressBar comp incomp
  setProgressTextAlignment bar AlignLeft

  aref <- newIORef ""
  tref <- newIORef ""

  let play = PlaybackStatus
          { playbackWidget = bar
          , playbackArtist = aref
          , playbackTitle  = tref
          }

  return (play,bar)

setPlaybackArtist :: PlaybackStatus -> String -> IO ()
setPlaybackArtist p artist = do
  writeIORef (playbackArtist p) artist
  title <- readIORef (playbackTitle p)
  setProgressText (playbackWidget p) (artist ++ " - " ++ title)

setPlaybackTitle :: PlaybackStatus -> String -> IO ()
setPlaybackTitle p title = do
  writeIORef (playbackTitle p) title
  artist <- readIORef (playbackArtist p)
  setProgressText (playbackWidget p) (artist ++ " - " ++ title)

setPlaybackProgress :: PlaybackStatus -> Int -> IO ()
setPlaybackProgress p = setProgress (playbackWidget p)
