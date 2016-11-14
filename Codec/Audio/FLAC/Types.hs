-- |
-- Module      :  Codec.Audio.FLAC.Types
-- Copyright   :  © 2016 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Types exported by the library.

module Codec.Audio.FLAC.Types
  ( FlacMetaSettings (..)
  , MetaChainStatus (..) )
where

import Data.Default.Class

-- | Settings that control how metadata is written in FLAC file.

data FlacMetaSettings = FlacMetaSettings
  { flacMetaSortPadding   :: Bool
  , flacMetaUsePadding    :: Bool
  , flacMetaPreserveStats :: Bool
  } deriving (Show, Read, Eq, Ord)

instance Default FlacMetaSettings where
  def = FlacMetaSettings
    { flacMetaSortPadding   = True
    , flacMetaUsePadding    = True
    , flacMetaPreserveStats = True }

-- | Enumeration of meta chain statuses.

data MetaChainStatus
  = MetaChainStatusOK
    -- ^ The chain is in the normal OK state.
  | MetaChainStatusIllegalInput
    -- ^ The data passed into a function violated the function's usage
    -- criteria.
  | MetaChainStatusErrorOpeningFile
    -- ^ The chain could not open the target file.
  | MetaChainStatusNotFlacFile
    -- ^ The chain could not find the FLAC signature at the start of the
    -- file.
  | MetaChainStatusNotWritable
    -- ^ The chain tried to write to a file that was not writable.
  | MetaChainStatusBadMetadata
    -- ^ The chain encountered input that does not conform to the FLAC
    -- metadata specification.
  | MetaChainStatusReadError
    -- ^ The chain encountered an error while reading the FLAC file.
  | MetaChainStatusSeekError
    -- ^ The chain encountered an error while seeking in the FLAC file.
  | MetaChainStatusWriteError
    -- ^ The chain encountered an error while writing the FLAC file.
  | MetaChainStatusRenameError
    -- ^ The chain encountered an error renaming the FLAC file.
  | MetaChainStatusUnlinkError
    -- ^ The chain encountered an error removing the temporary file.
  | MetaChainStatusMemoryAllocationError
    -- ^ Memory allocation failed.
  | MetaChainStatusInternalError
    -- ^ The caller violated an assertion or an unexpected error occurred.
  | MetaChainStatusInvalidCallbacks
    -- ^ One or more of the required callbacks was NULL.
  | MetaChainStatusReadWriteMismatch
    -- ^ This error occurs when read and write methods do not match (i.e.
    -- when if you read with callbacks, you should also write with
    -- callbacks).
  | MetaChainStatusWrongWriteCall
    -- ^ Should not ever happen when you use this binding.
  deriving (Show, Read, Eq, Ord, Bounded, Enum)
