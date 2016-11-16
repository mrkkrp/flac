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
  , EncoderState (..) )
where

import Foreign
import Data.Void

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
  | EnocderStateMemoryAllocationError
    -- ^ Memory allocation failed.
  deriving (Show, Read, Eq, Ord, Bounded, Enum)
