-- |
-- Module      :  Codec.Audio.FLAC.StreamEncoder
-- Copyright   :  © 2016–2018 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
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
-- The implementation uses the reference implementation of FLAC—libFLAC (C
-- library) under the hood. This means you'll need at least version 1.3.0 of
-- libFLAC (released 26 May 2013) installed for the binding to work.
--
-- The binding works with minimal overhead compared to the C implementation.
-- Encoding speed is equal to that of @flac@ command line tool. Memory
-- consumption is minimal and remains constant regardless of size of file to
-- decode.

{-# LANGUAGE RecordWildCards #-}

module Codec.Audio.FLAC.StreamEncoder
  ( EncoderSettings (..)
  , EncoderException (..)
  , EncoderInitStatus (..)
  , EncoderState (..)
  , encodeFlac )
where

import Codec.Audio.FLAC.StreamEncoder.Internal
import Codec.Audio.FLAC.StreamEncoder.Internal.Helpers
import Codec.Audio.FLAC.StreamEncoder.Internal.Types
import Codec.Audio.FLAC.Util
import Codec.Audio.Wave
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class (MonadIO (..))
import Data.Bool (bool)
import Data.Default.Class
import Data.List.NonEmpty (NonEmpty (..))
import Data.Word
import System.Directory

-- | Parameters of the stream encoder. Note that the 'encoderCompression'
-- parameter influences a number of other parameters on its own as specified
-- here
-- <https://xiph.org/flac/api/group__flac__stream__encoder.html#gae49cf32f5256cb47eecd33779493ac85>.
-- The parameters that it sets automatically are wrapped in 'Maybe's, so you
-- can choose whether to use the value that is set by 'encoderCompression'
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
  , encoderLooseMidSideStereo :: !(Maybe Bool)
    -- ^ Set to 'True' to enable adaptive switching between mid-side and
    -- left-right encoding on stereo input. Set to 'False' to use exhaustive
    -- searching. Setting this to 'True' requires 'encoderDoMidSideStereo'
    -- to also be set to 'True' in order to have any effect. Default value:
    -- 'Nothing'.
  , encoderApodization :: !(Maybe (NonEmpty ApodizationFunction))
    -- ^ Sets the apodization function(s) the encoder will use when
    -- windowing audio data for LPC analysis. Up to 32 functions are kept,
    -- the rest are dropped. Import
    -- "Codec.Audio.FLAC.StreamEncoder.Apodization" to bring apodization
    -- functions in scope. Default value: 'Nothing'.
  , encoderMaxLpcOrder :: !(Maybe Word32)
    -- ^ Set maximum LPC order, or 0 to use the fixed predictors. Default
    -- value: 'Nothing'.
  , encoderQlpCoeffPrecision :: !(Maybe Word32)
    -- ^ Set the precision in bits of the quantized linear predictor
    -- coefficients, or 0 to let the encoder select it based on the
    -- blocksize. Default value: 'Nothing'.
  , encoderDoQlpCoeffPrecisionSearch :: !(Maybe Bool)
    -- ^ Set to 'False' to use only the specified quantized linear predictor
    -- coefficient precision, or 'True' to search neighboring precision
    -- values and use the best one. Default value: 'Nothing'.
  , encoderDoExhaustiveModelSearch :: !(Maybe Bool)
    -- ^ Set to 'False' to let the encoder estimate the best model order
    -- based on the residual signal energy, or 'True' to force the encoder
    -- to evaluate all order models and select the best. Default value:
    -- 'Nothing'.
  , encoderResidualPartitionOrders :: !(Maybe (Word32, Word32))
    -- ^ Set the minimum and maximum partition order to search when coding
    -- the residual. The partition order determines the context size in the
    -- residual. The context size will be approximately @blocksize / (2 ^
    -- order)@. Set both min and max values to 0 to force a single context,
    -- whose Rice parameter is based on the residual signal variance.
    -- Otherwise, set a min and max order, and the encoder will search all
    -- orders, using the mean of each context for its Rice parameter, and
    -- use the best. Default: 'Nothing'.
  } deriving (Show, Read, Eq, Ord)

instance Default EncoderSettings where
  def = EncoderSettings
    { encoderCompression               = 5
    , encoderBlockSize                 = 0
    , encoderVerify                    = False
    , encoderDoMidSideStereo           = Nothing
    , encoderLooseMidSideStereo        = Nothing
    , encoderApodization               = Nothing
    , encoderMaxLpcOrder               = Nothing
    , encoderQlpCoeffPrecision         = Nothing
    , encoderDoQlpCoeffPrecisionSearch = Nothing
    , encoderDoExhaustiveModelSearch   = Nothing
    , encoderResidualPartitionOrders   = Nothing }

-- | Encode a WAVE file or RF64 file to native FLAC.
--
-- If the input file is not a valid WAVE file, 'WaveException' will be
-- thrown. 'EncoderException' is thrown when underlying FLAC encoder reports
-- a problem.
--
-- Please note that there are a number of limitations on parameters of input
-- audio stream (imposed by current reference FLAC implementation):
--
--     * Number of channels may be only 1–8 inclusive.
--     * Supported values for bits per sample are 4–24 inclusive.
--     * Acceptable sample rate lies in the range 1–655350 inclusive.

encodeFlac :: MonadIO m
  => EncoderSettings   -- ^ Encoder settings
  -> FilePath          -- ^ File to encode
  -> FilePath          -- ^ Where to save the resulting FLAC file
  -> m ()
encodeFlac EncoderSettings {..} ipath' opath' = liftIO . withEncoder $ \e -> do
  ipath <- makeAbsolute ipath'
  opath <- makeAbsolute opath'
  wave  <- readWaveFile ipath
  case waveSampleFormat wave of
    SampleFormatPcmInt _ -> return ()
    fmt -> throwIO (EncoderInvalidSampleFormat fmt)
  let channels      = fromIntegral (waveChannels wave)
      bitsPerSample = fromIntegral (waveBitsPerSample wave)
      sampleRate    = waveSampleRate wave
      totalSamples  = waveSamplesTotal wave
  liftInit (encoderSetChannels      e channels)
  liftInit (encoderSetBitsPerSample e bitsPerSample)
  liftInit (encoderSetSampleRate    e sampleRate)
  liftInit (encoderSetCompression   e encoderCompression)
  liftInit (encoderSetBlockSize     e encoderBlockSize)
  liftInit (encoderSetVerify        e encoderVerify)
  forM_ encoderDoMidSideStereo
    (liftInit . encoderSetDoMidSideStereo e)
  forM_ encoderLooseMidSideStereo
    (liftInit . encoderSetLooseMidSideStereo e)
  forM_ encoderApodization
    (liftInit . encoderSetApodization e . renderApodizationSpec)
  forM_ encoderMaxLpcOrder
    (liftInit . encoderSetMaxLpcOrder e)
  forM_ encoderQlpCoeffPrecision
    (liftInit . encoderSetQlpCoeffPrecision e)
  forM_ encoderDoQlpCoeffPrecisionSearch
    (liftInit . encoderSetDoQlpCoeffPrecisionSearch e)
  forM_ encoderDoExhaustiveModelSearch
    (liftInit . encoderSetDoExhaustiveModelSearch e)
  forM_ encoderResidualPartitionOrders
    (liftInit . encoderSetMinResidualPartitionOrder e . fst)
  forM_ encoderResidualPartitionOrders
    (liftInit . encoderSetMaxResidualPartitionOrder e . snd)
  -- Set the estimate (which is likely correct), to avoid rewrite of
  -- STREAMINFO metadata block after encoding.
  liftInit (encoderSetTotalSamplesEstimate e totalSamples)
  withTempFile' opath $ \otemp -> do
    initStatus <- encoderInitFile e otemp
    case initStatus of
      EncoderInitStatusOK -> return ()
      status -> throwIO (EncoderInitFailed status)
    liftBool e $ encoderProcessHelper e
      (fromIntegral $ waveDataOffset wave)
      (waveDataSize wave)
      ipath
    liftBool e (encoderFinish e)
    renameFile otemp opath

----------------------------------------------------------------------------
-- Helpers

-- | Execute an initializing action that returns 'False' on failure and take
-- care of error reporting. In case of trouble, @'EncoderInitFailed'
-- 'EncoderInitStatusAlreadyInitialized'@ is thrown.

liftInit :: IO Bool -> IO ()
liftInit m = liftIO m >>= bool t (return ())
  where
    t = throwIO (EncoderInitFailed EncoderInitStatusAlreadyInitialized)

-- | Execute an action that returns 'False' on failure into taking care of
-- error reporting. In case of trouble @'EncoderFailed'@ with encoder status
-- attached is thrown.

liftBool :: Encoder -> IO Bool -> IO ()
liftBool encoder m = liftIO m >>= bool (throwState encoder) (return ())

-- | Get 'EncoderState' from given 'Encoder' and throw it immediately.

throwState :: Encoder -> IO a
throwState = encoderGetState >=> throwIO . EncoderFailed
