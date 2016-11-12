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

module Codec.Audio.FLAC.Metadata
  (  )
where

-- https://xiph.org/flac/api/group__flac__metadata.html

-- This should be built around the concept of file — the libFLAC library
-- does not give us alternatives to that.

-- We should use: Level 2: Read-write access to all metadata blocks. This
-- level is write- efficient in all cases, but uses more memory since all
-- metadata for the whole file is read into memory and manipulated before
-- writing out again.
