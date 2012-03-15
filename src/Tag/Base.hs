{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}

module Tag.Base (
    FileRef
  , openFileRef
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
import Foreign.ForeignPtr.Safe
    (ForeignPtr,FinalizerPtr,newForeignPtr,withForeignPtr)
import Foreign.Marshal.Array (lengthArray0,copyArray)
import qualified Control.Exception as E
import qualified Data.ByteString.Internal as SI
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

type FileRef = ForeignPtr FileRef_

openFileRef :: FilePath -> IO (Maybe FileRef)
openFileRef path =
  withCString path $ \ c_path -> do
    f     <- c_open c_path
    valid <- c_isValid f
    if fromIntegral valid == (0 :: Int)
      then Nothing <$  c_freeFileRef_ f
      else Just    <$> newForeignPtr c_fin_freeFileRef_ f

unpackString_ :: IO (Ptr String_) -> IO T.Text
unpackString_ create =
  E.bracket create c_freeString $ \ str -> do
    src <- c_toCString str
    len <- lengthArray0 0 src
    T.decodeUtf8 `fmap` SI.create len (\ dst -> copyArray dst src len)

type GetString = Ptr FileRef_ -> IO (Ptr String_)

-- | Gets the content of the artist field.
getArtist :: FileRef -> IO T.Text
getArtist fp = withForeignPtr fp (unpackString_ . c_artist)

foreign import ccall safe "c_artist"
  c_artist :: GetString

-- | Gets the content of the title field.
getTitle :: FileRef -> IO T.Text
getTitle fp = withForeignPtr fp (unpackString_ . c_title)

foreign import ccall safe "c_title"
  c_title :: GetString

-- | Get the content of the album field.
getAlbum :: FileRef -> IO T.Text
getAlbum fp = withForeignPtr fp (unpackString_ . c_album)

foreign import ccall safe "c_album"
  c_album :: GetString

-- | Get the content of the comment field.
getComment :: FileRef -> IO T.Text
getComment fp = withForeignPtr fp (unpackString_ . c_comment)

foreign import ccall safe "c_comment"
  c_comment :: GetString

-- | Get the content of the genre field.
getGenre :: FileRef -> IO T.Text
getGenre fp = withForeignPtr fp (unpackString_ . c_genre)

foreign import ccall safe "c_genre"
  c_genre :: GetString


type GetInt = Ptr FileRef_ -> IO CInt

getYear :: FileRef -> IO Int
getYear fp = withForeignPtr fp (fmap fromIntegral . c_year)

foreign import ccall safe "c_year"
  c_year :: GetInt

getTrack :: FileRef -> IO Int
getTrack fp = withForeignPtr fp (fmap fromIntegral . c_track)

foreign import ccall safe "c_track"
  c_track :: GetInt


-- File Inerface ---------------------------------------------------------------

data FileRef_

foreign import ccall safe "c_open"
  c_open :: CString -> IO (Ptr FileRef_)

foreign import ccall safe "c_freeFileRef"
  c_freeFileRef_ :: Ptr FileRef_ -> IO ()

foreign import ccall safe "&c_freeFileRef"
  c_fin_freeFileRef_ :: FinalizerPtr FileRef_

foreign import ccall safe "c_isValid"
  c_isValid :: Ptr FileRef_ -> IO CChar


-- String Management -----------------------------------------------------------

data String_

foreign import ccall safe "c_toCString"
  c_toCString :: Ptr String_ -> IO (Ptr Word8)

foreign import ccall safe "c_freeString"
  c_freeString :: Ptr String_ -> IO ()
