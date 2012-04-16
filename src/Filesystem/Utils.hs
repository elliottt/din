module Filesystem.Utils (
    -- * File Extensions
    Extension
  , normalizeExtension

    -- * File Paths
  , isVisible

    -- * Re-exported
  , takeExtension
  ) where

import Data.Char (toLower)
import System.FilePath (takeExtension)


type Extension = String

normalizeExtension :: Extension -> Extension
normalizeExtension  = map toLower

isVisible :: FilePath -> Bool
isVisible ""      = False
isVisible ('.':_) = False
isVisible _       = True
