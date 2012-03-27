{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Storage where

import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH


share
  [ mkPersist sqlSettings
  , mkMigrate "migrateAll"
  ]
  [persistUpperCase|

Song
        url String
        SongURL url

Info
        songId 

|]
