-- |
-- Module      :  Codec.Audio.FLAC.StreamDecoder
-- Copyright   :  © 2016–2017 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- The module contains a Haskell interface to FLAC stream decoder.
--
-- === How to use this module
--
-- Just call the 'decodeFlac' function with 'DecoderSettings', input and
-- output file names. The 'encodeFlac' function can produce vanilla WAVE
-- and RF64.
--
-- === Low-level details
--
-- The implementation uses the reference implementation of FLAC — libFLAC (C
-- library) under the hood. This means you'll need at least version 1.3.0 of
-- libFLAC (released 26 May 2013) installed for the binding to work.

{-# LANGUAGE RecordWildCards #-}

module Codec.Audio.FLAC.StreamDecoder
  ( DecoderSettings (..)
  , DecoderException (..)
  , DecoderInitStatus (..)
  , DecoderState (..)
  , decodeFlac )
where

import Codec.Audio.FLAC.Metadata
import Codec.Audio.FLAC.StreamDecoder.Internal
import Codec.Audio.FLAC.StreamDecoder.Internal.Helpers
import Codec.Audio.FLAC.StreamDecoder.Internal.Types
import Codec.Audio.FLAC.Util
import Codec.Audio.Wave
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class (MonadIO (..))
import Data.Bool (bool)
import Data.Default.Class
import Data.Function
import Data.IORef
import Foreign
import System.Directory
import System.IO

-- | Parameters of stream decoder.

data DecoderSettings = DecoderSettings
  { decoderMd5Checking :: Bool
    -- ^ If 'True', the decoder will compute the MD5 signature of the
    -- unencoded audio data while decoding and compare it to the signature
    -- from the STREAMINFO block. Default value: 'False'.
  , decoderWaveFormat :: WaveFormat
    -- ^ This specifies WAVE format in which to save the decoded file. You
    -- can choose between 'WaveVanilla' and 'WaveRF64'; choose the latter if
    -- uncompressed file is expected to be longer than 4 Gb. Default value:
    -- 'WaveVanilla'.
  }

instance Default DecoderSettings where
  def = DecoderSettings
    { decoderMd5Checking = False
    , decoderWaveFormat  = WaveVanilla }

-- | Decode a FLAC file to WAVE.
--
-- 'DecoderException' is thrown when underlying FLAC decoder reports a
-- problem.

decodeFlac :: MonadIO m
  => DecoderSettings   -- ^ Decoder settings
  -> FilePath          -- ^ File to decode
  -> FilePath          -- ^ Where to save the resulting WAVE file
  -> m ()
decodeFlac DecoderSettings {..} ipath' opath' = liftIO . withDecoder $ \d -> do
  ipath <- makeAbsolute ipath'
  opath <- makeAbsolute opath'
  liftInit (decoderSetMd5Checking d decoderMd5Checking)
  (maxBlockSize, wave) <- runFlacMeta def ipath $ do
    let waveFileFormat   = decoderWaveFormat
        waveDataOffset   = 0
        waveDataSize     = 0
        waveOtherChunks  = []
    waveSampleRate <- retrieve SampleRate
    waveSampleFormat <- SampleFormatPcmInt . fromIntegral
      <$> retrieve BitsPerSample
    waveChannelMask <- retrieve ChannelMask
    waveSamplesTotal <- retrieve TotalSamples
    maxBlockSize <- fromIntegral <$> retrieve MaxBlockSize
    return (maxBlockSize, Wave {..})
  let bufferSize = maxBlockSize * fromIntegral (waveBlockAlign wave) + 1
  withTempFile' opath $ \otemp ->
    bracket (mallocBytes bufferSize) free $ \buffer -> do
      initStatus <- decoderInitHelper d ipath buffer
      case initStatus of
        DecoderInitStatusOK -> return ()
        status -> throwIO (DecoderInitFailed status)
      liftBool d (decoderProcessUntilEndOfMetadata d)
      processedRef <- newIORef (0 :: Word64)
      writeWaveFile otemp wave $ \h -> fix $ \nextOne -> do
        processed <- readIORef processedRef
        unless (processed == waveSamplesTotal wave) $ do
          liftBool d (decoderProcessSingle d)
          frameSize <- fromIntegral <$> decoderGetBlockSize d
          let toGrab = frameSize * fromIntegral (waveBlockAlign wave)
          -- FIXME This method relies on the fact that host architecture is
          -- little-endian. It won't work on big-endian architectures. Right
          -- now it's fine with me, but you can open a PR to add big-endian
          -- support.
          hPutBuf h buffer toGrab
          modifyIORef' processedRef (+ fromIntegral frameSize)
          nextOne
      liftBool d (decoderFinish d)
      renameFile otemp opath

----------------------------------------------------------------------------
-- Helpers

-- | Execute an initializing action that returns 'False' on failure and take
-- care of error reporting. In case of trouble, @'DecoderInitFailed'
-- 'DecoderInitStatusAlreadyInitialized'@ is thrown.

liftInit :: IO Bool -> IO ()
liftInit m = liftIO m >>= bool t (return ())
  where
    t = throwIO (DecoderInitFailed DecoderInitStatusAlreadyInitialized)

-- | Execute an action that returns 'False' on failure into taking care of
-- error reporting. In case of trouble @'EncoderFailed'@ with encoder status
-- attached is thrown.

liftBool :: Decoder -> IO Bool -> IO ()
liftBool encoder m = liftIO m >>= bool (throwState encoder) (return ())

-- | Get 'EncoderState' from given 'Encoder' and throw it immediately.

throwState :: Decoder -> IO a
throwState = decoderGetState >=> throwIO . DecoderFailed
