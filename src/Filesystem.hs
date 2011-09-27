module Filesystem (
    -- * File Extensions
    Extension
  , normalizeExtension

    -- * Re-exported
  , takeExtension
  ) where

import Data.Char (toLower)
import System.FilePath (takeExtension)


type Extension = String

normalizeExtension :: Extension -> Extension
normalizeExtension  = map toLower
