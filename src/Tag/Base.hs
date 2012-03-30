{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}

module Tag.Base (
    withTags
  , Tag()
  , getTitle
  , getArtist
  , getAlbum
  , getComment
  , getGenre
  , getYear
  , getTrack
  ) where

import Control.Monad ((<=<))
import Data.Word (Word8)
import Foreign.C.String (CString,withCString)
import Foreign.C.Types (CInt(..))
import Foreign.Marshal.Array (lengthArray0,copyArray)
import Foreign.Ptr (Ptr,nullPtr)
import qualified Control.Exception as E
import qualified Data.ByteString.Internal as SI
import qualified Data.Text as T
import qualified Data.Text.Encoding as T


-- TagLib Files ----------------------------------------------------------------

data TagLib_File

foreign import ccall "taglib_file_new"
  c_taglib_file_new :: CString -> IO (Ptr TagLib_File)

foreign import ccall "taglib_file_free"
  c_taglib_file_free :: Ptr TagLib_File -> IO ()

foreign import ccall "taglib_file_is_valid"
  c_taglib_file_is_valid :: Ptr TagLib_File -> IO CInt

foreign import ccall "taglib_file_tag"
  c_taglib_file_tag :: Ptr TagLib_File -> IO (Ptr Tag)

foreign import ccall "taglib_tag_free_strings"
  c_taglib_free_strings :: IO ()


-- | Process the Tag pointer for a file.
withTags :: FilePath -> (Ptr Tag -> IO a) -> IO (Maybe a)
withTags path k =
  withCString path                             $ \ c_path ->
  E.bracket (c_taglib_file_new c_path) cleanup $ \ c_file ->
    if nullPtr == c_file
       then return Nothing
       else do
         res <- c_taglib_file_is_valid c_file
         if 0 == res
            then return Nothing
            else fmap Just (k =<< c_taglib_file_tag c_file)

  where
  cleanup c_file = do
    c_taglib_free_strings
    c_taglib_file_free c_file


-- Tags ------------------------------------------------------------------------

-- | Abstract Tag object.
data Tag


type GetString = Ptr Tag -> IO (Ptr Word8)

unpackString :: GetString -> Ptr Tag -> IO T.Text
unpackString k c_tag = do
  c_str <- k c_tag
  len   <- lengthArray0 0 c_str
  T.decodeUtf8 `fmap` SI.create len (\ dst -> copyArray dst c_str len)

getTitle :: Ptr Tag -> IO T.Text
getTitle  = unpackString c_taglib_tag_title

foreign import ccall "taglib_tag_title"
  c_taglib_tag_title :: GetString

getArtist :: Ptr Tag -> IO T.Text
getArtist  = unpackString c_taglib_tag_artist

foreign import ccall "taglib_tag_artist"
  c_taglib_tag_artist :: GetString

getAlbum :: Ptr Tag -> IO T.Text
getAlbum  = unpackString c_taglib_tag_album

foreign import ccall "taglib_tag_album"
  c_taglib_tag_album :: GetString

getComment :: Ptr Tag -> IO T.Text
getComment  = unpackString c_taglib_tag_comment

foreign import ccall "taglib_tag_comment"
  c_taglib_tag_comment :: GetString

getGenre :: Ptr Tag -> IO T.Text
getGenre  = unpackString c_taglib_tag_genre

foreign import ccall "taglib_tag_genre"
  c_taglib_tag_genre :: GetString


type GetInt = Ptr Tag -> IO CInt

unpackInt :: GetInt -> Ptr Tag -> IO Int
unpackInt k c_tag = fromIntegral `fmap` k c_tag

getYear :: Ptr Tag -> IO Int
getYear  = unpackInt c_taglib_tag_year

foreign import ccall "taglib_tag_year"
  c_taglib_tag_year :: GetInt

getTrack :: Ptr Tag -> IO Int
getTrack  = unpackInt c_taglib_tag_track

foreign import ccall "taglib_tag_track"
  c_taglib_tag_track :: GetInt
