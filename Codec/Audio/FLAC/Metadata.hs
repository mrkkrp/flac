-- |
-- Module      :  Codec.Audio.FLAC.Metadata
-- Copyright   :  © 2016 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Operations on FLAC metadata.

{-# LANGUAGE TypeFamilies #-}

module Codec.Audio.FLAC.Metadata
  (  )
where

import Codec.Audio.FLAC.Metadata.Internal
import Control.Monad.Except
import Control.Monad.Reader
import Data.Proxy

-- https://xiph.org/flac/api/group__flac__metadata.html

-- This should be built around the concept of file — the libFLAC library
-- does not give us alternatives to that.

-- We should use: Level 2: Read-write access to all metadata blocks. This
-- level is write- efficient in all cases, but uses more memory since all
-- metadata for the whole file is read into memory and manipulated before
-- writing out again.

data FlacMetaError = FlacMetaError

data FlacMeta a = ExceptT FlacMetaError (ReaderT MetaIterator IO a)

-- functor
-- applicative
-- monad
-- foldable
-- traversable

data FlacMetaSettings -- what to do with padding, etc.

runFlacMeta :: MonadIO m => FlacMetaSettings -> FlacMeta a -> m (Either FlacMetaError a)
runFlacMeta = undefined -- TODO don't forget to use bracket

data MetaValueMarker
  = MinBlockSize

class MetaValue a where -- class should not be public as stuff necessary to
  -- implement its methods won't be available to end user
  type MetaValueType a :: *
  -- what if we could have associated types of kind constraint that would
  -- determine what actions can be performed?

-- the crud api

get :: MetaValue a => a -> FlacMeta (MetaValueType a)
get = undefined

add :: MetaValue a => a -> MetaValueType a -> FlacMeta ()
add = undefined

set :: MetaValue a => a -> MetaValueType a -> FlacMeta ()
set = undefined

update :: MetaValue a => a -> (MetaValueType a -> MetaValueType a) -> FlacMeta (MetaValueType a)
update = undefined

delete :: MetaValue a => a -> FlacMeta ()
delete = undefined

wipe :: FlacMeta () -- deletes all meta data
wipe = undefined

-- min/max blocksize
-- min/max framesize
-- sample rate
-- channels
-- bits per sample
-- total samples
-- md5 sum (byte string)

-- we can calculate duration as a floating point number

-- application data (identified by four-byte id, provide enumeration)

-- seektable

-- vorbis comment we need common fields to be pre-defined enumeration to
-- avoid typos

-- cue sheet

-- picture as sequence of bytes, the stuff in docs

----------------------------------------------------------------------------
-- Helpers

-- need lifters that would take care of constant checking if everything is
-- OK after every action, they should signal correct errors automatically
