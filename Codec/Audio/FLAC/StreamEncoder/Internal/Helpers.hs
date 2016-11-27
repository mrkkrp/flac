-- |
-- Module      :  Codec.Audio.FLAC.StreamEncoder.Internal.Helpers
-- Copyright   :  © 2016 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Wrappers around helpers written to help work with stream encoder.

{-# LANGUAGE ForeignFunctionInterface #-}

module Codec.Audio.FLAC.StreamEncoder.Internal.Helpers
  ( encoderGetWaveInfo
  , encoderProcessWave )
where

import Codec.Audio.FLAC.StreamEncoder.Internal.Types
import Foreign
import Foreign.C.String

-- | Get parameters of given WAVE file in the following order:
--     * number of channels;
--     * number of bits per sample;
--     * sample rate in Hz.
--
-- If given file is not recognized as a WAVE file, 'Nothing' is returned.

encoderGetWaveInfo :: FilePath -> IO (Maybe (Word32, Word32, Word32))
encoderGetWaveInfo path = withCString path $ \pathPtr ->
  alloca $ \channelsPtr ->
    alloca $ \bpsPtr ->
      alloca $ \sampleRatePtr -> do
        res <- c_encoder_get_wave_info pathPtr channelsPtr bpsPtr sampleRatePtr
        if res
          then return Nothing
          else do
            channels      <- peek channelsPtr
            bitsPerSample <- peek bpsPtr
            sampleRate    <- peek sampleRatePtr
            return $ Just (channels, bitsPerSample, sampleRate)

foreign import ccall unsafe "FLAC__stream_encoder_get_wave_info"
  c_encoder_get_wave_info
    :: CString -> Ptr Word32 -> Ptr Word32 -> Ptr Word32 -> IO Bool

-- | Encode given WAVE file, return 'False' in case of failure.

encoderProcessWave :: Encoder -> FilePath -> IO Bool
encoderProcessWave encoder path =
  withCString path (c_encoder_process_wave encoder)

foreign import ccall unsafe "FLAC__stream_encoder_process_wav"
  c_encoder_process_wave :: Encoder -> CString -> IO Bool
