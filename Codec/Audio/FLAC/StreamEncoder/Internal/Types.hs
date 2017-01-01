-- |
-- Module      :  Codec.Audio.FLAC.StreamEncoder.Internal.Types
-- Copyright   :  © 2016–2017 Mark Karpov
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
  , EncoderException (..)
  , ApodizationFunction (..) )
where

import Codec.Audio.Wave (SampleFormat)
import Control.Exception
import Data.Void
import Foreign
import Numeric.Natural

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
    -- ^ The encoder has an invalid setting for the sample rate.
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

-- | Enumeration of encoder states.

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

data EncoderException
  = EncoderInvalidSampleFormat SampleFormat
    -- ^ Input WAVE file had this sample format, which is not supported
    -- (usually happens with floating point samples right now).
  | EncoderInitFailed EncoderInitStatus
    -- ^ Encoder initialization failed.
  | EncoderFailed EncoderState
    -- ^ Encoder failed.
  deriving (Eq, Show, Read)

instance Exception EncoderException

-- | Supported apodization functions.

data ApodizationFunction
  = Bartlett
  | BartlettHann
  | Blackman
  | BlackmanHarris4Term92Db
  | Connes
  | Flattop
  | Gauss Double
    -- ^ The parameter is standard deviation @STDDEV@, @0 < STDDEV <= 0.5@.
  | Hamming
  | Hann
  | KaiserBessel
  | Nuttall
  | Rectangle
  | Triangle
  | Tukey Double
    -- ^ The parameter is the fraction of the window that is tapered @P@,
    -- @0 <= P <= 1@. @P == 0@ corresponds to 'Rectangle' and @P = 1@
    -- corresponds to 'Hann'.
  | PartialTukey Natural (Maybe (Double, Maybe Double))
    -- ^ The parameters are a series of small windows (all treated
    -- separately). The three parameters are @n@, @ov@ and @P@. @n@ is the
    -- number of functions to add, @ov@ is the overlap of the windows. @P@
    -- is the fraction of the window that is tapered, like with a regular
    -- tukey window. The function can be specified with only a number, a
    -- number and an overlap, or a number, an overlap and a @P@. @ov@ should
    -- be smaller than 1 and can be negative.
  | PunchoutTukey Natural (Maybe (Double, Maybe Double))
    -- ^ The parameters are a series of windows that have a hole in them. In
    -- this way, the predictor is constructed with only a part of the block,
    -- which helps in case a block consists of dissimilar parts. All said
    -- about the parameters in the comment for 'PartialTukey' applies here,
    -- with the exception that @ov@ is the overlap in the gaps in this case.
  | Welch
  deriving (Eq, Show, Read, Ord)
