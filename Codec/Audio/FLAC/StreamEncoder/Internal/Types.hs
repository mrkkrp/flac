-- |
-- Module      :  Codec.Audio.FLAC.StreamEncoder.Internal.Types
-- Copyright   :  © 2016 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Mostly non-public stream encoder-specific helper types.

module Codec.Audio.FLAC.StreamEncoder.Internal.Types
  ( Encoder (..)
  , EncoderInitStatus (..)
  , EncoderState (..)
  , FlacEncoderException (..)
  , AudioInfo (..)
  , AudioFormat (..) )
where

import Control.Exception
import Data.Void
import Foreign

-- | An opaque newtype wrapper around 'Ptr' 'Void', serves to represent
-- pointer to encoder instance.

newtype Encoder = Encoder (Ptr Void)

-- | Status of encoder initialization process.

data EncoderInitStatus
  = EncoderInitStatusOK
    -- ^ Initialization was successful.
  | EncoderInitStatusEncoderError
    -- ^ General failure to set up encoder.
  | EncoderInitStatusUnsupportedCointainer
    -- ^ The library was not compiled with support for the given container
    -- format.
  | EncoderInitStatusInvalidCallbacks
    -- ^ A required callback was not supplied.
  | EncoderInitStatusInvalidNumberOfChannels
    -- ^ The encoder has an invalid setting for number of channels.
  | EncoderInitStatusInvalidBitsPerSample
    -- ^ The encoder has an invalid setting for bits-per-sample. FLAC
    -- supports 4-32 bps but the reference encoder currently supports only
    -- up to 24 bps.
  | EncoderInitStatusInvalidSampleRate
    -- ^ The encoder has an invalid setting for the input sample rate.
  | EncoderInitStatusInvalidBlockSize
    -- ^ The encoder has an invalid setting for the block size.
  | EncoderInitStatusInvalidMaxLpcOrder
    -- ^ The encoder has an invalid setting for the maximum LPC order.
  | EncoderInitStatusInvalidQlpCoeffPrecision
    -- ^ The encoder has an invalid setting for the precision of the
    -- quantized linear predictor coefficients.
  | EncoderInitStatusBlockSizeTooSmallForLpcOrder
    -- ^ The specified block size is less than the maximum LPC order.
  | EncoderInitStatusNotStreamable
    -- ^ The encoder is bound to the Subset but other settings violate it.
  | EncoderInitStatusInvalidMetadata
    -- ^ The metadata input to the encoder is invalid (should never happen
    -- with this binding).
  | EncoderInitStatusAlreadyInitialized
    -- ^ Initialization was attempted on already initialized encoder.
  deriving (Show, Read, Eq, Ord, Bounded, Enum)

-- | Enumeration of encoder statuses.

data EncoderState
  = EncoderStateOK
    -- ^ The encoder is in the normal OK state and samples can be processed.
  | EncoderStateUninitialized
    -- ^ The encoder is in the uninitialized state.
  | EncoderStateOggError
    -- ^ An error occurred in the underlying Ogg layer.
  | EncoderStateVerifyDecoderError
    -- ^ An error occurred in the underlying verify stream decoder.
  | EncoderStateVerifyMismatchInAudioData
    -- ^ The verify decoder detected a mismatch between the original audio
    -- signal and the decoded audio signal.
  | EncoderStateClientError
    -- ^ One of the callbacks returned a fatal error.
  | EncoderStateIOError
    -- ^ An I\/O error occurred while opening\/reading\/writing a file.
  | EncoderStateFramingError
    -- ^ An error occurred while writing the stream.
  | EncoderStateMemoryAllocationError
    -- ^ Memory allocation failed.
  deriving (Show, Read, Eq, Ord, Bounded, Enum)

-- | Exception that is thrown when encoding fails for some reason.

data FlacEncoderException
  = FlacEncoderInitFailed EncoderInitStatus
    -- ^ Encoder initialization failed.
  | FlacEncoderFailed EncoderState
    -- ^ Encoder failed.
  deriving (Eq, Show, Read)

instance Exception FlacEncoderException

-- | An internal record holding parameters of input file.

data AudioInfo = AudioInfo
  { audioInfoFormat        :: !AudioFormat -- ^ Format of file
  , audioInfoChannels      :: !Word32      -- ^ Number of channels
  , audioInfoBitsPerSample :: !Word32      -- ^ Bits per sample
  , audioInfoSampleRate    :: !Word32      -- ^ Sample rate
  , audioInfoFileSize      :: !Integer     -- ^ File size
  } deriving (Show, Read, Eq, Ord)

-- | An internal data type that represents all input file formats the
-- library can work with.

data AudioFormat
  = FormatWave
  | FormatWave64
  | FormatRF64
  deriving (Show, Read, Eq, Ord, Bounded, Enum)
