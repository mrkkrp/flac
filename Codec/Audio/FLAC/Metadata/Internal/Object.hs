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
  , objectSeektableIsLegal )
where

import Codec.Audio.FLAC.Metadata.Internal.Types
import Codec.Audio.FLAC.Util
import Data.Word (Word32)
import Foreign.C.Types

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
