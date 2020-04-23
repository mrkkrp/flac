-- |
-- Module      :  Codec.Audio.FLAC.StreamEncoder.Internal.Types
-- Copyright   :  © 2016–present Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Mostly non-public stream encoder-specific helper types.
module Codec.Audio.FLAC.StreamEncoder.Internal.Types
  ( Encoder (..),
    EncoderInitStatus (..),
    EncoderState (..),
    EncoderException (..),
    ApodizationFunction (..),
  )
where

import Codec.Audio.Wave (SampleFormat)
import Control.Exception
import Data.Void
import Foreign
import Numeric.Natural

-- | An opaque newtype wrapper around @'Ptr' 'Void'@, serves to represent a
-- pointer to an encoder instance.
newtype Encoder = Encoder (Ptr Void)

-- | Status of encoder initialization process.
data EncoderInitStatus
  = -- | Initialization was successful.
    EncoderInitStatusOK
  | -- | General failure to set up encoder.
    EncoderInitStatusEncoderError
  | -- | The library was not compiled with support for the given container
    -- format.
    EncoderInitStatusUnsupportedCointainer
  | -- | A required callback was not supplied.
    EncoderInitStatusInvalidCallbacks
  | -- | The encoder has an invalid setting for the number of channels.
    EncoderInitStatusInvalidNumberOfChannels
  | -- | The encoder has an invalid setting for the bits-per-sample. FLAC
    -- supports 4-32 bps but the reference encoder currently supports only
    -- up to 24 bps.
    EncoderInitStatusInvalidBitsPerSample
  | -- | The encoder has an invalid setting for the sample rate.
    EncoderInitStatusInvalidSampleRate
  | -- | The encoder has an invalid setting for the block size.
    EncoderInitStatusInvalidBlockSize
  | -- | The encoder has an invalid setting for the maximum LPC order.
    EncoderInitStatusInvalidMaxLpcOrder
  | -- | The encoder has an invalid setting for the precision of the
    -- quantized linear predictor coefficients.
    EncoderInitStatusInvalidQlpCoeffPrecision
  | -- | The specified block size is less than the maximum LPC order.
    EncoderInitStatusBlockSizeTooSmallForLpcOrder
  | -- | The encoder is bound to the Subset but other settings violate it.
    EncoderInitStatusNotStreamable
  | -- | The metadata input to the encoder is invalid (should never happen
    -- with this binding).
    EncoderInitStatusInvalidMetadata
  | -- | Initialization was attempted on already initialized encoder.
    EncoderInitStatusAlreadyInitialized
  deriving (Show, Read, Eq, Ord, Bounded, Enum)

-- | Enumeration of encoder states.
data EncoderState
  = -- | The encoder is in the normal OK state and samples can be processed.
    EncoderStateOK
  | -- | The encoder is in the uninitialized state.
    EncoderStateUninitialized
  | -- | An error occurred in the underlying Ogg layer.
    EncoderStateOggError
  | -- | An error occurred in the underlying verify stream decoder.
    EncoderStateVerifyDecoderError
  | -- | The verify decoder detected a mismatch between the original audio
    -- signal and the decoded audio signal.
    EncoderStateVerifyMismatchInAudioData
  | -- | One of the callbacks returned a fatal error.
    EncoderStateClientError
  | -- | An I\/O error occurred while opening\/reading\/writing a file.
    EncoderStateIOError
  | -- | An error occurred while writing the stream.
    EncoderStateFramingError
  | -- | Memory allocation failed.
    EncoderStateMemoryAllocationError
  deriving (Show, Read, Eq, Ord, Bounded, Enum)

-- | Exception that is thrown when encoding fails for some reason.
data EncoderException
  = -- | Input WAVE file had this sample format, which is not supported
    -- (usually happens with floating point samples right now).
    EncoderInvalidSampleFormat SampleFormat
  | -- | Encoder initialization failed.
    EncoderInitFailed EncoderInitStatus
  | -- | Encoder failed.
    EncoderFailed EncoderState
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
  | -- | The parameter is standard deviation @STDDEV@, @0 < STDDEV <= 0.5@.
    Gauss Double
  | Hamming
  | Hann
  | KaiserBessel
  | Nuttall
  | Rectangle
  | Triangle
  | -- | The parameter is the fraction of the window that is tapered @P@,
    -- @0 <= P <= 1@. @P == 0@ corresponds to 'Rectangle' and @P = 1@
    -- corresponds to 'Hann'.
    Tukey Double
  | -- | The parameters are a series of small windows (all treated
    -- separately). The three parameters are @n@, @ov@ and @P@. @n@ is the
    -- number of functions to add, @ov@ is the overlap of the windows. @P@
    -- is the fraction of the window that is tapered, like with a regular
    -- tukey window. The function can be specified with only a number, a
    -- number and an overlap, or a number, an overlap and a @P@. @ov@ should
    -- be smaller than 1 and can be negative.
    PartialTukey Natural (Maybe (Double, Maybe Double))
  | -- | The parameters are a series of windows that have a hole in them. In
    -- this way, the predictor is constructed with only a part of the block,
    -- which helps in case a block consists of dissimilar parts. All said
    -- about the parameters in the comment for 'PartialTukey' applies here,
    -- with the exception that @ov@ is the overlap in the gaps in this case.
    PunchoutTukey Natural (Maybe (Double, Maybe Double))
  | Welch
  deriving (Eq, Show, Read, Ord)
