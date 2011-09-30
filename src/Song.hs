module Song where

data Song = Song
  { songPath   :: FilePath
  , songName   :: String
  , songArtist :: String
  , songAlbum  :: String
  } deriving (Show)
