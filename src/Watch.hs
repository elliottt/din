module Watch (
    watcher
  ) where

import Din.Monad
import Tag (SongInfo,getSongInfo)

import Control.Concurrent (Chan,newChan,readChan,writeChan)
import Control.Monad (forever)
import System.INotify
    (INotify,initINotify,WatchDescriptor,addWatch,EventVariety(..),Event(..))

watcher :: [FilePath] -> Din ()
watcher paths = do
  logInfo  ("Watching: " ++ show paths)
  chan <- io (setupNotify paths)
  forever (processChange =<< io (readChan chan))

data Change
  = AddFile    FilePath
  | RemoveFile FilePath
    deriving (Show)

processChange :: Change -> Din ()
processChange ch = case ch of
  AddFile path    -> logInfo ("Adding file: " ++ path)
  RemoveFile path -> logInfo ("Removing file: " ++ path)


-- INotify ---------------------------------------------------------------------

setupNotify :: [FilePath] -> IO (Chan Change)
setupNotify paths = do

  notify <- initINotify
  chan   <- newChan

  mapM_ (watchDir chan notify) paths

  return chan

eventTypes :: [EventVariety]
eventTypes  =
  [ Create
  , Delete
  , MoveIn
  , MoveOut
  ]

watchDir :: Chan Change -> INotify -> FilePath -> IO WatchDescriptor
watchDir chan notify path = addWatch notify eventTypes path (processEvent chan)

processEvent :: Chan Change -> Event -> IO ()
processEvent chan evt = case evt of
  Created  isDir path   | not isDir -> writeChan chan (AddFile path)
  MovedIn  isDir path _ | not isDir -> writeChan chan (AddFile path)
  Deleted  isDir path   | not isDir -> writeChan chan (RemoveFile path)
  MovedOut isDir path _ | not isDir -> writeChan chan (RemoveFile path)

  _ -> return ()
