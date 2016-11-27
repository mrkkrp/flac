-- |
-- Module      :  Codec.Audio.FLAC.StreamEncoder.Internal
-- Copyright   :  © 2016 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Low-level Haskell wrapper around FLAC stream encoder API, see:
--
-- <https://xiph.org/flac/api/group__flac__stream__encoder.html>

{-# LANGUAGE ForeignFunctionInterface #-}

module Codec.Audio.FLAC.StreamEncoder.Internal
  ( withEncoder
  , encoderSetChannels
  , encoderSetBitsPerSample
  , encoderSetSampleRate
  , encoderSetCompression
  , encoderSetBlockSize
  , encoderSetDoMidSideStereo
    -- TODO FLAC__stream_encoder_set_loose_mid_side_stereo
    -- TODO FLAC__stream_encoder_set_apodization
    -- TODO FLAC__stream_encoder_set_max_lpc_order
    -- TODO FLAC__stream_encoder_set_qlp_coeff_precision
    -- TODO FLAC__stream_encoder_set_do_qlp_coeff_prec_search
    -- TODO FLAC__stream_encoder_set_do_escape_coding
    -- TODO FLAC__stream_encoder_set_do_exhaustive_model_search
    -- TODO FLAC__stream_encoder_set_min_residual_partition_order
    -- TODO FLAC__stream_encoder_set_max_residual_partition_order
    -- TODO FLAC__stream_encoder_set_rice_parameter_search_dist
  , encoderSetVerify
  , encoderGetState
  , encoderInitFile
  , encoderFinish )
where

import Codec.Audio.FLAC.StreamEncoder.Internal.Types
import Codec.Audio.FLAC.Util
import Control.Monad.Catch
import Data.Void
import Foreign
import Foreign.C.String
import Foreign.C.Types

-- | Create and use an 'Encoder'. The encoder is guaranteed to be freed even
-- in case of exception.
--
-- If memory for the encoder cannot be allocated, corresponding
-- 'FlacEncoderException' is raised.

withEncoder :: (Encoder -> IO a) -> IO a
withEncoder f = bracket encoderNew (mapM_ encoderDelete) $ \mencoder ->
  case mencoder of
    Nothing -> throwM
      (FlacEncoderFailed EncoderStateMemoryAllocationError)
    Just x -> f x

-- | Create a new stream encoder instance with default settings. In the case
-- of memory allocation problem 'Nothing' is returned.

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

encoderSetCompression :: Encoder -> Word32 -> IO Bool
encoderSetCompression encoder level =
  c_encoder_set_compression_level encoder (fromIntegral level)

foreign import ccall unsafe "FLAC__stream_encoder_set_compression_level"
  c_encoder_set_compression_level :: Encoder -> CUInt -> IO Bool

-- | Set the blocksize to use while encoding.

encoderSetBlockSize :: Encoder -> Word32 -> IO Bool
encoderSetBlockSize encoder blockSize =
  c_encoder_set_blocksize encoder (fromIntegral blockSize)

foreign import ccall unsafe "FLAC__stream_encoder_set_blocksize"
  c_encoder_set_blocksize :: Encoder -> CUInt -> IO Bool

-- | Set to 'True' to enable mid-side encoding on stereo input.

encoderSetDoMidSideStereo :: Encoder -> Bool -> IO Bool
encoderSetDoMidSideStereo = c_encoder_set_do_mid_side_stereo

foreign import ccall unsafe "FLAC_stream_encoder_set_do_mid_side_stereo"
  c_encoder_set_do_mid_side_stereo :: Encoder -> Bool -> IO Bool

-- | Set the “verify” flag. If true, the encoder will verify it's own
-- encoded output by feeding it through an internal decoder and comparing
-- the original signal against the decoded signal. If a mismatch occurs, the
-- process call will return false. Note that this will slow the encoding
-- process by the extra time required for decoding and comparison.

encoderSetVerify :: Encoder -> Bool -> IO Bool
encoderSetVerify = c_encoder_set_verify

foreign import ccall unsafe "FLAC__stream_encoder_set_verify"
  c_encoder_set_verify :: Encoder -> Bool -> IO Bool

-- | Get current encoder state.

encoderGetState :: Encoder -> IO EncoderState
encoderGetState = fmap toEnum' . c_encoder_get_state

foreign import ccall unsafe "FLAC__stream_encoder_get_state"
  c_encoder_get_state :: Encoder -> IO CUInt

-- | Initialize the encoder instance to encode native FLAC files.

encoderInitFile
  :: Encoder           -- ^ Uninitialized encoder instance
  -> FilePath          -- ^ Name of file to encode to
  -> IO EncoderInitStatus
encoderInitFile encoder path =
  withCString path $ \cstr ->
    toEnum' <$> c_encoder_init_file encoder cstr nullPtr nullPtr

foreign import ccall unsafe "FLAC__stream_encoder_init_file"
  c_encoder_init_file :: Encoder -> CString -> Ptr Void -> Ptr Void -> IO CUInt

-- | Finish the encoding process and release resources (also resets encoder
-- and its settings). Return 'False' in case of trouble.

encoderFinish :: Encoder -> IO Bool
encoderFinish = c_encoder_finish

foreign import ccall unsafe "FLAC__stream_encoder_finish"
  c_encoder_finish :: Encoder -> IO Bool
