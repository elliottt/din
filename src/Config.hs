module Config where

data Config = Config
  { configWatchPaths :: [FilePath]
  } deriving Show
