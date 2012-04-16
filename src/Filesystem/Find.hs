module Filesystem.Find where

import Filesystem.Utils
import Utils

import Control.Monad (foldM)
import System.Directory (getDirectoryContents,doesDirectoryExist)
import System.FilePath ((</>))


findFiles :: FilePath -> (FilePath -> Bool) -> IO [FilePath]
findFiles base pred = loop base
  where
  loop path = do
    (matches,dirs) <- partition =<< getDirectoryContents path
    rest           <- mapM (loop . (path </>)) dirs
    return (concat (map (path </>) matches : rest))

  partition = foldM step ([],[])
    where
    step acc@(ms,ds) path = do
      isDir <- doesDirectoryExist path
      case path of
        "."           -> return acc
        ".."          -> return acc
        _ | isDir     -> return (ms,path:ds)
          | pred path -> return (path:ms,ds)
          | otherwise -> return acc

