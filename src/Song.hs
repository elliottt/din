module Song where

type Artist = String

type Album = String

data Song = Song
  { songPath   :: FilePath
  , songName   :: String
  , songArtist :: Artist
  , songAlbum  :: Album
  } deriving (Show)
