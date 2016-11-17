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
  , objectDelete )
where

import Codec.Audio.FLAC.Metadata.Internal.Types
import Codec.Audio.FLAC.Util
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
