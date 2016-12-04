-- |
-- Module      :  Codec.Audio.FLAC.StreamEncoder
-- Copyright   :  © 2016 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- The module contains a Haskell interface to FLAC stream encoder.
--
-- === How to use this module
--
-- Just call the 'encodeFlac' function with 'EncoderSettings', input and
-- output file names. The 'encodeFlac' function only encodes vanilla WAVE
-- and RF64.
--
-- === Low-level details
--
-- The implementation uses the reference implementation of FLAC — libFLAC (C
-- library) under the hood. This means you'll need at least version 1.3.0 of
-- libFLAC (released 26 May 2013) installed for the binding to work.

{-# LANGUAGE RecordWildCards #-}

module Codec.Audio.FLAC.StreamEncoder
  ( EncoderSettings (..)
  , EncoderInitStatus (..)
  , EncoderState (..)
  , encodeFlac )
where

import Codec.Audio.FLAC.StreamEncoder.Internal
import Codec.Audio.FLAC.StreamEncoder.Internal.Helpers
import Codec.Audio.FLAC.StreamEncoder.Internal.Types
import Codec.Audio.Wave
import Control.Exception
import Control.Monad.Except
import Data.Bool (bool)
import Data.Default.Class
import Data.Word

-- | Parameters of stream encoder and input to encode. Note that the
-- 'encoderCompression' parameter influences a number of other parameters on
-- its own as specified here
-- <https://xiph.org/flac/api/group__flac__stream__encoder.html#gae49cf32f5256cb47eecd33779493ac85>.
-- The parameters that it sets automatically are wrapped in 'Maybe's, so you
-- can choose to use the value that is set by 'encoderCompression'
-- specifying 'Nothing' (default), or use something specific by passing a
-- value inside 'Just'. Thorough understanding of the FLAC format is
-- necessary to achieve good results, though.

data EncoderSettings = EncoderSettings
  { encoderCompression :: !Word32
    -- ^ Compression level [0..8], default is 5.
  , encoderBlockSize :: !Word32
    -- ^ Block size, default is 0.
  , encoderVerify :: !Bool
    -- ^ Verify result (slower), default is 'False'.
  , encoderDoMidSideStereo :: !(Maybe Bool)
    -- ^ Enable mid-side encoding on stereo input. The number of channels
    -- must be 2 for this to have any effect. Default value: 'Nothing'.
  }

instance Default EncoderSettings where
  def = EncoderSettings
    { encoderCompression     = 5
    , encoderBlockSize       = 0
    , encoderVerify          = False
    , encoderDoMidSideStereo = Nothing }

-- | Encode a WAVE file or RF64 file to native FLAC.
--
-- If the input file is not a valid WAVE file, 'WaveException' will be
-- thrown. 'FlacEncoderException' is thrown when underlying FLAC encoder
-- reports a problem.
--
-- Please note that there are a number of limitations on parameters of input
-- audio stream (imposed by current reference FLAC implementation):
--
--     * Number of channels may be only 1–8 inclusive.
--     * Supported values for bits per sample are 4–24 inclusive.
--     * Acceptable sample rate lies in the range 1–655350 inclusive.

encodeFlac
  :: MonadIO m
  => EncoderSettings   -- ^ Encoder settings
  -> FilePath          -- ^ File to encode
  -> FilePath          -- ^ Where to save the resulting FLAC file
  -> m ()
encodeFlac EncoderSettings {..} ipath opath = liftIO . withEncoder $ \e -> do
  wave <- readWaveFile ipath
  let channels      = fromIntegral (waveChannels wave)
      bitsPerSample = fromIntegral (waveBitsPerSample wave)
      sampleRate    = waveSampleRate wave
  liftInit (encoderSetChannels      e channels)
  liftInit (encoderSetBitsPerSample e bitsPerSample)
  liftInit (encoderSetSampleRate    e sampleRate)
  liftInit (encoderSetCompression   e encoderCompression)
  liftInit (encoderSetBlockSize     e encoderBlockSize)
  liftInit (encoderSetVerify        e encoderVerify)
  forM_ encoderDoMidSideStereo (liftInit . encoderSetVerify e)
  -- TODO add more parameters here later
  initStatus <- encoderInitFile e opath
  case initStatus of
    EncoderInitStatusOK -> return ()
    status -> throwIO (FlacEncoderInitFailed status)
  liftBool e $ encoderProcessHelper e
    (waveFileFormat wave)
    (fromIntegral $ waveDataOffset wave)
    (waveDataSize wave)
    ipath
    opath
  liftBool e (encoderFinish e)

----------------------------------------------------------------------------
-- Helpers

-- | Execute an initializing action that returns 'False' on failure and take
-- care of error reporting. In case of trouble, @'FlacEncoderInitFailed'
-- 'EncoderInitStatusAlreadyInitialized'@ is thrown.

liftInit :: IO Bool -> IO ()
liftInit m = liftIO m >>= bool t (return ())
  where
    t = throwIO (FlacEncoderInitFailed EncoderInitStatusAlreadyInitialized)

-- | Execute an action that returns 'False' on failure into taking care of
-- error reporting. In case of trouble @'EncoderFailed'@ with encoder status
-- attached is thrown.

liftBool :: Encoder -> IO Bool -> IO ()
liftBool encoder m = liftIO m >>= bool (throwStatus encoder) (return ())

-- | Get 'EncoderState' from given 'Encoder' and throw it immediately.

throwStatus :: Encoder -> IO a
throwStatus = encoderGetState >=> throwIO . FlacEncoderFailed
