{-# LANGUAGE ForeignFunctionInterface #-}

-- |
-- Module      :  Codec.Audio.FLAC.StreamDecoder.Internal.Helpers
-- Copyright   :  © 2016–present Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Wrappers around helpers written to help work with the stream decoder.
module Codec.Audio.FLAC.StreamDecoder.Internal.Helpers
  ( decoderInitHelper,
  )
where

import Codec.Audio.FLAC.StreamDecoder.Internal.Types
import Codec.Audio.FLAC.Util
import Data.Void
import Foreign
import Foreign.C.String
import Foreign.C.Types

-- | Initialize decoder to decode FLAC file and register buffer where
-- decoded audio data will go (which must be big enough). Return
-- 'DecoderInitStatus'.
decoderInitHelper ::
  -- | 'Decoder' to use
  Decoder ->
  -- | FLAC file to decode
  FilePath ->
  -- | Buffer where to write decoded audio data
  Ptr Void ->
  -- | Sample rate, bits per sample, channels
  IO DecoderInitStatus
decoderInitHelper decoder ipath buffer =
  withCString ipath $ \ipathPtr ->
    toEnum'
      <$> c_decoder_init_helper
        decoder -- stream decoder
        ipathPtr -- name of FLAC file to decode
        buffer -- output buffer

foreign import ccall unsafe "FLAC__stream_decoder_init_helper"
  c_decoder_init_helper ::
    Decoder ->
    CString ->
    Ptr Void ->
    IO CUInt
