{-# LANGUAGE DeriveDataTypeable #-}

module Tag (
    SongInfo(..)
  , getSongInfo
  ) where

import Tag.Base

import Control.Applicative ((<$>),(<*>))
import Data.Typeable (Typeable)
import qualified Control.Exception as E
import qualified Data.Text as T

data SongInfo =  SongInfo
  { songArtist  :: !T.Text
  , songTitle   :: !T.Text
  , songAlbum   :: !T.Text
  , songComment :: !T.Text
  , songGenre   :: !T.Text
  , songYear    :: !Int
  , songTrack   :: !Int
  } deriving (Show)

data SongException = FileNotFound FilePath
    deriving (Show,Typeable)

instance E.Exception SongException

getSongInfo :: FilePath -> IO SongInfo
getSongInfo path = do
  mb <- openFileRef path
  case mb of
    Just f  -> mkSongInfo f
    Nothing -> E.throwIO (FileNotFound path)

mkSongInfo :: FileRef -> IO SongInfo
mkSongInfo f = SongInfo
           <$> getArtist  f
           <*> getTitle   f
           <*> getAlbum   f
           <*> getComment f
           <*> getGenre   f
           <*> getYear    f
           <*> getTrack   f
