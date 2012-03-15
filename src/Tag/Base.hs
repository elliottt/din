{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}

module Tag.Base (
    FileRef
  , withFileRef
  , getArtist
  , getTitle
  , getAlbum
  , getComment
  , getGenre
  , getYear
  , getTrack
  ) where

import Control.Applicative ((<$>),(<$))
import Data.Char (toUpper)
import Data.Word (Word8)
import Foreign.Ptr (Ptr,castPtr)
import Foreign.C.String (CString,withCString,peekCString)
import Foreign.C.Types (CChar(..),CInt(..))
import Foreign.Marshal.Array (lengthArray0,copyArray)
import qualified Control.Exception as E
import qualified Data.ByteString.Internal as SI
import qualified Data.Text as T
import qualified Data.Text.Encoding as T


-- File Inerface ---------------------------------------------------------------

type FileRef = Ptr FileRef_

data FileRef_

withFileRef :: FilePath -> (FileRef -> IO a) -> IO (Maybe a)
withFileRef path body =
  withCString path                       $ \ c_path ->
  E.bracket (c_open c_path) c_freeFileRef_ $ \ ptr    -> do
    valid <- c_isValid ptr
    if fromIntegral valid == (0 :: Int)
      then return Nothing
      else Just <$> body ptr

foreign import ccall safe "c_open"
  c_open :: CString -> IO FileRef

foreign import ccall safe "c_freeFileRef"
  c_freeFileRef_ :: FileRef -> IO ()

foreign import ccall safe "c_isValid"
  c_isValid :: FileRef -> IO CChar


-- Tag Interface ---------------------------------------------------------------

unpackString_ :: IO (Ptr String_) -> IO T.Text
unpackString_ create =
  E.bracket create c_freeString $ \ str -> do
    src <- c_toCString str
    len <- lengthArray0 0 src
    T.decodeUtf8 `fmap` SI.create len (\ dst -> copyArray dst src len)

type GetString = FileRef -> IO (Ptr String_)

-- | Gets the content of the artist field.
getArtist :: FileRef -> IO T.Text
getArtist  = unpackString_ . c_artist

foreign import ccall safe "c_artist"
  c_artist :: GetString

-- | Gets the content of the title field.
getTitle :: FileRef -> IO T.Text
getTitle  = unpackString_ . c_title

foreign import ccall safe "c_title"
  c_title :: GetString

-- | Get the content of the album field.
getAlbum :: FileRef -> IO T.Text
getAlbum  = unpackString_ . c_album

foreign import ccall safe "c_album"
  c_album :: GetString

-- | Get the content of the comment field.
getComment :: FileRef -> IO T.Text
getComment  = unpackString_ . c_comment

foreign import ccall safe "c_comment"
  c_comment :: GetString

-- | Get the content of the genre field.
getGenre :: FileRef -> IO T.Text
getGenre  = unpackString_ . c_genre

foreign import ccall safe "c_genre"
  c_genre :: GetString

type GetInt = Ptr FileRef_ -> IO CInt

getYear :: FileRef -> IO Int
getYear  = fmap fromIntegral . c_year

foreign import ccall safe "c_year"
  c_year :: GetInt

getTrack :: FileRef -> IO Int
getTrack  = fmap fromIntegral . c_track

foreign import ccall safe "c_track"
  c_track :: GetInt


-- String Management -----------------------------------------------------------

data String_

foreign import ccall safe "c_toCString"
  c_toCString :: Ptr String_ -> IO (Ptr Word8)

foreign import ccall safe "c_freeString"
  c_freeString :: Ptr String_ -> IO ()
