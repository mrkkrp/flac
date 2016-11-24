-- |
-- Module      :  Codec.Audio.FLAC.Metadata.Internal.Object
-- Copyright   :  © 2016 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Wrappers for functions to work with metadata objects, see:
--
-- <https://xiph.org/flac/api/group__flac__metadata__object.html>.

{-# LANGUAGE ForeignFunctionInterface #-}

module Codec.Audio.FLAC.Metadata.Internal.Object
  ( objectNew
  , objectDelete
  , objectSeektableResizePoints
  , objectSeektableIsLegal
  , objectPictureSetMimeType
  , objectPictureSetDescription
  , objectPictureSetData
  , objectPictureIsLegal )
where

import Codec.Audio.FLAC.Metadata.Internal.Types
import Codec.Audio.FLAC.Util
import Data.ByteString (ByteString)
import Data.Text (Text)
import Foreign
import Foreign.C.String
import Foreign.C.Types
import qualified Data.ByteString.Unsafe as B

-- | Create a new metadata object given its type.

objectNew :: MetadataType -> IO (Maybe Metadata)
objectNew = fmap maybePtr . c_object_new . fromEnum'

foreign import ccall unsafe "FLAC__metadata_object_new"
  c_object_new :: CUInt -> IO Metadata

-- | Free a metadata object.

objectDelete :: Metadata -> IO ()
objectDelete = c_object_delete

foreign import ccall unsafe "FLAC__metadata_object_delete"
  c_object_delete :: Metadata -> IO ()

-- | Resize the seekpoint array. In case of trouble return 'False'.

objectSeektableResizePoints :: Metadata -> Word32 -> IO Bool
objectSeektableResizePoints block newSize =
  c_object_seektable_resize_points block (fromIntegral newSize)

foreign import ccall unsafe "FLAC__metadata_object_seektable_resize_points"
  c_object_seektable_resize_points :: Metadata -> CUInt -> IO Bool

-- | Check a seek table to see if it conforms to the FLAC specification.
-- Return 'False' if the seek table is illegal.

objectSeektableIsLegal :: Metadata -> IO Bool
objectSeektableIsLegal = c_object_seektable_is_legal

foreign import ccall unsafe "FLAC__metadata_object_seektable_is_legal"
  c_object_seektable_is_legal :: Metadata -> IO Bool

-- | Check a picture and return description of what is wrong, otherwise
-- 'Nothing'.

objectPictureIsLegal :: Metadata -> IO (Maybe Text)
objectPictureIsLegal block = alloca $ \cstrPtr -> do
  res <- c_object_picture_is_legal block cstrPtr
  if res
    then return Nothing
    else Just <$> (peek cstrPtr >>= peekCStringText)

foreign import ccall unsafe "FLAC__metadata_object_picture_is_legal"
  c_object_picture_is_legal :: Metadata -> Ptr CString -> IO Bool

-- | Set the MIME type of a given picture block.

objectPictureSetMimeType :: Metadata -> Text -> IO Bool
objectPictureSetMimeType block mimeType =
  withCStringText mimeType $ \cstr ->
    c_object_picture_set_mime_type block cstr True

foreign import ccall unsafe "FLAC__metadata_object_picture_set_mime_type"
  c_object_picture_set_mime_type :: Metadata -> CString -> Bool -> IO Bool

-- | Set the description of a given picture block.

objectPictureSetDescription :: Metadata -> Text -> IO Bool
objectPictureSetDescription block desc =
  withCStringText desc $ \cstr ->
    c_object_picture_set_description block cstr True

foreign import ccall unsafe "FLAC__metadata_object_picture_set_description"
  c_object_picture_set_description :: Metadata -> CString -> Bool -> IO Bool

-- | Set the picture data of a given picture block.

objectPictureSetData :: Metadata -> ByteString -> IO Bool
objectPictureSetData block data' =
  B.unsafeUseAsCStringLen data' $ \(dataPtr, dataSize) ->
    c_object_picture_set_data block dataPtr (fromIntegral dataSize) True

foreign import ccall unsafe "FLAC__metadata_object_picture_set_data"
  c_object_picture_set_data :: Metadata -> CString -> Word32 -> Bool -> IO Bool
