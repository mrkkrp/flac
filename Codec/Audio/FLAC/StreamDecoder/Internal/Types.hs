-- |
-- Module      :  Codec.Audio.FLAC.StreamDecoder.Internal.Types
-- Copyright   :  © 2016–2017 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Mostly non-public stream decoder-specific helper types.

module Codec.Audio.FLAC.StreamDecoder.Internal.Types
  ( Decoder (..)
  , DecoderInitStatus (..)
  , DecoderState (..)
  , DecoderException (..)
  , ChannelAssignment (..) )
where

import Control.Exception
import Data.Void
import Foreign

-- | An opaque newtype wrapper around 'Ptr' 'Void', serves to represent
-- point to decoder instance.

newtype Decoder = Decoder (Ptr Void)

-- | Status of decoder initialization process.

data DecoderInitStatus
  = DecoderInitStatusOK
    -- ^ Initialization was successful.
  | DecoderInitStatusUnsupportedContainer
    -- ^ The library was not compiled with support for the given container
    -- format.
  | DecoderInitStatusInvalidCallbacks
    -- ^ A required callback was not supplied.
  | DecoderInitStatusMemoryAllocationError
    -- ^ An error occurred allocating memory.
  | DecoderInitStatusErrorOpeningFile
    -- ^ fopen() failed.
  | DecoderInitStatusAlreadyInitialized
    -- ^ Initialization was attempted on already initialized decoder.
  deriving (Show, Read, Eq, Ord, Bounded, Enum)

-- | Enumeration of decoder states.

data DecoderState
  = DecoderStateSearchForMetadata
    -- ^ The decoder is ready to search for metadata.
  | DecoderStateReadMetadata
    -- ^ The decoder is ready to or is in the process of reading metadata.
  | DecoderStateSearchForFrameSync
    -- ^ The decoder is ready to or is in the process of searching for the
    -- frame sync code.
  | DecoderStateReadFrame
    -- ^ The decoder is ready to or is in the process of reading a frame.
  | DecoderStateEndOfStream
    -- ^ The decoder has reached the end of the stream.
  | DecoderStateOggError
    -- ^ An error occurred in the underlying Ogg layer.
  | DecoderStateSeekError
    -- ^ An error occurred while seeking. The decoder must be flushed or
    -- reset before decoding can continue.
  | DecoderStateAborted
    -- ^ The decoder was aborted by the read callback.
  | DecoderStateMemoryAllocationError
    -- ^ An error occurred allocating memory. The decoder is in an invalid
    -- state and can no longer be used.
  | DecoderStateUnititialized
    -- ^ The decoder is in the uninitialized state.
  deriving (Show, Read, Eq, Ord, Bounded, Enum)

-- | Exception that is thrown when decoding fails for some reason.

data DecoderException
  = DecoderInitFailed DecoderInitStatus
    -- ^ Decoder initialization failed.
  | DecoderFailed DecoderState
    -- ^ Decoder failed.
  deriving (Eq, Show, Read)

instance Exception DecoderException

-- | An enumeration of the available channel assignments.

data ChannelAssignment
  = ChannelAssignmentIndependent -- ^ Independent channels
  | ChannelAssignmentLeftSide    -- ^ Left+side stereo
  | ChannelAssignmentRightSide   -- ^ Right+side stereo
  | ChannelAssignmentMidSide     -- ^ Mid+side stereo
  deriving (Show, Read, Eq, Ord, Bounded, Enum)
