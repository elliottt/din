{-# LANGUAGE OverloadedStrings #-}

module Storage.Query where

import Storage.Schema

import Data.String (fromString)
import Data.Int (Int64)
import Database.Persist
import Database.Persist.Query.Join
import Database.Persist.Store
import Database.Persist.Sqlite
import qualified Data.Text as T


newtype DNF = DNF
  { unDNF :: [CNF]
  } deriving (Show)

newtype CNF = CNF
  { unCNF :: [Query]
  } deriving (Show)

data Query
  = MatchPrefix      InfoType String
  | MatchIntLt  Bool InfoType Int
  | MatchIntGt  Bool InfoType Int
  | MatchIntEq       InfoType Int
    deriving (Show)


-- Compilation via DNF ---------------------------------------------------------

--compileDNF :: DNF -> [[(T.Text,PersistValue)]]
compileDNF (DNF cnfs) = map compileCNF cnfs

--compileCNF :: CNF -> [(T.Text,PersistValue)]
compileCNF (CNF qs) =  concatMap compileQuery qs

--compileQuery :: Query -> [(T.Text,PersistValue)]
compileQuery q = case q of
  MatchPrefix ty str -> withLabel ty
    [ Filter
      { filterField  = InfoString
      , filterValue  = Left (str ++ "%")
      , filterFilter = BackendSpecificFilter "LIKE"
      }
    ]

  MatchIntGt eq ty i
    | eq             -> withLabel ty [ InfoNumber >=. i ]
    | otherwise      -> withLabel ty [ InfoNumber >.  i ]
  MatchIntLt eq ty i
    | eq             -> withLabel ty [ InfoNumber <=. i ]
    | otherwise      -> withLabel ty [ InfoNumber <.  i ]
  MatchIntEq    ty i -> withLabel ty [ InfoNumber ==. i ]

withLabel :: InfoType -> [Filter Info] -> [Filter Info]
withLabel ty rest = (InfoLabel ==. ty) : rest
