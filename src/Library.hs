module Library where

import Filesystem
import Song

import System.Directory (getDirectoryContents,doesDirectoryExist)


-- | The song library.
data Library = Library


-- | Store the library in the din directory.
saveLibrary :: Library -> IO ()
saveLibrary _ = do
  putStrLn "Saving library"

-- | Add a file to the library.
addFile :: FilePath -> Library -> IO ()
addFile path _ = do
  putStrLn ("Adding: " ++ path)

-- | Add all visible files to the library.
addDirectory :: Bool -> FilePath -> Library -> IO ()
addDirectory recurse path0 lib = loop path0
  where
  loop path = do
    files <- getDirectoryContents path
    mapM_ step (filter isVisible files)

  step path = do
    b <- doesDirectoryExist path
    let body | b && recurse = loop path
             | b            = return ()
             | otherwise    = addFile path lib
    body


-- | A filter on the library, producing songs.
type Filter = Library -> IO [Song]
