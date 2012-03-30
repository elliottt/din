{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Storage where

import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH


data InfoType
  = InfoArtist
  | InfoTitle
  | InfoAlbum
  | InfoComment
  | InfoGenre
  | InfoYear
  | InfoTrack
    deriving (Show,Read,Eq,Ord)

derivePersistField "InfoType"

share
  [ mkPersist sqlSettings
  , mkMigrate "migrateAll"
  ]
  [persistUpperCase|

Song
        url             String
        SongURL         url

Info
        songId          SongId Eq
        infoType        InfoType
        infoString      String
        infoNum         Int

|]
