-- |
-- Module      :  Codec.Audio.FLAC.StreamEncoder.Internal
-- Copyright   :  © 2016 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Low-level Haskell wrapper around FLAC stream encoder.

{-# LANGUAGE ForeignFunctionInterface #-}

module Codec.Audio.FLAC.StreamEncoder.Internal
  ( -- * Types
    -- * Encoder
    encoderNew
  , encoderDelete
  , encoderSetChannels
  , encoderSetBitsPerSample
  , encoderSetSampleRate
  , encoderSetCompressionLevel
  , encoderSetVerify
  , encoderInitFile
  , encoderFinish
 )
where

import Codec.Audio.FLAC.Util
import Data.Void
import Foreign
import Foreign.C.String
import Foreign.C.Types

----------------------------------------------------------------------------
-- Types

newtype Encoder = Encoder (Ptr Void)

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
  deriving (Show, Read, Eq, Ord, Bounded, Enum)

data EncoderState
  = EncoderStateOK
    -- ^ The encoder is in the normal OK state and samples can be processed
  | EncoderStateUninitialized
    -- ^ The encoder is in the uninitialized state
  | EncoderStateOggError
    -- ^ An error occurred in the underlying Ogg layer
  | EncoderStateVerifyDecoderError
    -- ^ An error occurred in the underlying verify stream decoder
  | EncoderStateVerifyMismatchInAudioData
    -- ^ The verify decoder detected a mismatch between the original audio
    -- signal and the decoded audio signal
  | EncoderStateClientError
    -- ^ One of the callbacks returned a fatal error
  | EncoderStateIOError
    -- ^ An I\/O error occurred while opening\/reading\/writing a file
  | EncoderStateFramingError
    -- ^ An error occurred while writing the stream
  | EnocderStateMemoryAllocationError
    -- ^ Memory allocation failed
  deriving (Show, Read, Eq, Ord, Bounded, Enum)

----------------------------------------------------------------------------
-- Encoder

-- | Create a new stream encoder instance with default settings. If case of
-- memory allocation problem 'Nothing' is returned.

encoderNew :: IO (Maybe Encoder)
encoderNew = maybePtr <$> c_encoder_new

foreign import ccall unsafe "FLAC__stream_encoder_new"
  c_encoder_new :: IO Encoder

-- | Free an encoder instance.

encoderDelete :: Encoder -> IO ()
encoderDelete = c_encoder_delete

foreign import ccall unsafe "FLAC__stream_encoder_delete"
  c_encoder_delete :: Encoder -> IO ()

-- | Set the number of channels to be encoded. Return 'False' if encoder is
-- already initialized.

encoderSetChannels :: Encoder -> Word32 -> IO Bool
encoderSetChannels encoder channels =
  c_encoder_set_channels encoder (fromIntegral channels)

foreign import ccall unsafe "FLAC__stream_encoder_set_channels"
  c_encoder_set_channels :: Encoder -> CUInt -> IO Bool

-- | Set the same resolution of the input to be encoded. Return 'False' if
-- encoder is already initialized.

encoderSetBitsPerSample :: Encoder -> Word32 -> IO Bool
encoderSetBitsPerSample encoder bps =
  c_encoder_set_bits_per_sample encoder (fromIntegral bps)

foreign import ccall unsafe "FLAC__stream_encoder_set_bits_per_sample"
  c_encoder_set_bits_per_sample :: Encoder -> CUInt -> IO Bool

-- | Set the sample rate in Hz of the input to be encoded. Return 'False' if
-- encoder is already initialized.

encoderSetSampleRate :: Encoder -> Word32 -> IO Bool
encoderSetSampleRate encoder sampleRate =
  c_encoder_set_sample_rate encoder (fromIntegral sampleRate)

foreign import ccall unsafe "FLAC__stream_encoder_set_sample_rate"
  c_encoder_set_sample_rate :: Encoder -> CUInt -> IO Bool

-- | Set the compression level. The argument can range from 0 (fastest,
-- least compression) to 8 (slowest, most compression). A value higher than
-- 8 will be treated as 8. Return 'False' if encoder is already initialized.

encoderSetCompressionLevel :: Encoder -> Word32 -> IO Bool
encoderSetCompressionLevel encoder level =
  c_encoder_set_compression_level encoder (fromIntegral level)

foreign import ccall unsafe "FLAC__stream_encoder_set_compression_level"
  c_encoder_set_compression_level :: Encoder -> CUInt -> IO Bool

-- | Set the “verify” flag. If true, the encoder will verify it's own
-- encoded output by feeding it through an internal decoder and comparing
-- the original signal against the decoded signal. If a mismatch occurs, the
-- process call will return false. Note that this will slow the encoding
-- process by the extra time required for decoding and comparison.

encoderSetVerify :: Encoder -> Bool -> IO Bool
encoderSetVerify = c_encoder_set_verify

foreign import ccall unsafe "FLAC__stream_encoder_set_verify"
  c_encoder_set_verify :: Encoder -> Bool -> IO Bool

-- | Initialize the encoder instance to encode native FLAC files.

encoderInitFile :: Encoder -> FilePath -> IO EncoderInitStatus
encoderInitFile encoder path =
  withCString path $ \cstr ->
    toEnum' <$> c_encoder_init_file encoder cstr

foreign import ccall unsafe "FLAC__stream_encoder_init_file"
  c_encoder_init_file :: Encoder -> CString -> IO CUInt

-- TODO FLAC__stream_encoder_process probably will need some writing in C

-- | Finish the encoding process and release resources (also resets encoder
-- and its settings). Return 'False' in case of trouble.

encoderFinish :: Encoder -> IO Bool
encoderFinish = c_encoder_finish

foreign import ccall unsafe "FLAC__stream_encoder_finish"
  c_encoder_finish :: Encoder -> IO Bool
