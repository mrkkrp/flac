-- |
-- Module      :  Codec.Audio.FLAC.StreamDecoder.Internal.Types
-- Copyright   :  © 2016–present Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Mostly non-public stream decoder-specific helper types.
module Codec.Audio.FLAC.StreamDecoder.Internal.Types
  ( Decoder (..),
    DecoderInitStatus (..),
    DecoderState (..),
    DecoderException (..),
    ChannelAssignment (..),
  )
where

import Control.Exception
import Data.Void
import Foreign

-- | An opaque newtype wrapper around @'Ptr' 'Void'@, serves to represent
-- point to decoder instance.
newtype Decoder = Decoder (Ptr Void)

-- | Status of decoder initialization process.
data DecoderInitStatus
  = -- | Initialization was successful.
    DecoderInitStatusOK
  | -- | The library was not compiled with support for the given container
    -- format.
    DecoderInitStatusUnsupportedContainer
  | -- | A required callback was not supplied.
    DecoderInitStatusInvalidCallbacks
  | -- | An error occurred allocating memory.
    DecoderInitStatusMemoryAllocationError
  | -- | fopen() failed.
    DecoderInitStatusErrorOpeningFile
  | -- | Initialization was attempted on already initialized decoder.
    DecoderInitStatusAlreadyInitialized
  deriving (Show, Read, Eq, Ord, Bounded, Enum)

-- | Enumeration of decoder states.
data DecoderState
  = -- | The decoder is ready to search for metadata.
    DecoderStateSearchForMetadata
  | -- | The decoder is ready to or is in the process of reading metadata.
    DecoderStateReadMetadata
  | -- | The decoder is ready to or is in the process of searching for the
    -- frame sync code.
    DecoderStateSearchForFrameSync
  | -- | The decoder is ready to or is in the process of reading a frame.
    DecoderStateReadFrame
  | -- | The decoder has reached the end of the stream.
    DecoderStateEndOfStream
  | -- | An error occurred in the underlying Ogg layer.
    DecoderStateOggError
  | -- | An error occurred while seeking. The decoder must be flushed or
    -- reset before decoding can continue.
    DecoderStateSeekError
  | -- | The decoder was aborted by the read callback.
    DecoderStateAborted
  | -- | An error occurred allocating memory. The decoder is in an invalid
    -- state and can no longer be used.
    DecoderStateMemoryAllocationError
  | -- | The decoder is in the uninitialized state.
    DecoderStateUnititialized
  deriving (Show, Read, Eq, Ord, Bounded, Enum)

-- | Exception that is thrown when decoding fails for some reason.
data DecoderException
  = -- | Decoder initialization failed.
    DecoderInitFailed DecoderInitStatus
  | -- | Decoder failed.
    DecoderFailed DecoderState
  deriving (Eq, Show, Read)

instance Exception DecoderException

-- | An enumeration of the available channel assignments.
data ChannelAssignment
  = -- | Independent channels
    ChannelAssignmentIndependent
  | -- | Left+side stereo
    ChannelAssignmentLeftSide
  | -- | Right+side stereo
    ChannelAssignmentRightSide
  | -- | Mid+side stereo
    ChannelAssignmentMidSide
  deriving (Show, Read, Eq, Ord, Bounded, Enum)
