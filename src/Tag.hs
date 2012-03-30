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
  mb <- withTags path $ \ f ->
        SongInfo
    <$> getArtist  f
    <*> getTitle   f
    <*> getAlbum   f
    <*> getComment f
    <*> getGenre   f
    <*> getYear    f
    <*> getTrack   f
  case mb of
    Just info -> return info
    Nothing   -> E.throwIO (FileNotFound path)

