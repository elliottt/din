module Storage.Query where

import Storage.Schema

import Database.Persist
import Database.Persist.Sqlite


data Query
  = Conj Query Query
  | Disj Query Query
  | MatchPrefix InfoType String
  | MatchInt    InfoType MatchInt
    deriving (Show)

data MatchInt
  = Gt Bool Int
  | Lt Bool Int
  | Eq Int
    deriving (Show)
