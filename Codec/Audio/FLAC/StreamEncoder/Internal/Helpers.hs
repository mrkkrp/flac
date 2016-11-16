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
  ( encoderProcessWav )
where

import Codec.Audio.FLAC.StreamEncoder.Internal.Types
import Foreign.C.String

-- https://github.com/xiph/flac/blob/master/examples/c/encode/file/main.c

-- | Encode given WAV file, return 'False' in case of failure.

encoderProcessWav :: Encoder -> FilePath -> IO Bool
encoderProcessWav encoder path =
  withCString path (c_encoder_process_wav encoder)

foreign import ccall unsafe "FLAC__stream_encoder_process_wav"
  c_encoder_process_wav :: Encoder -> CString -> IO Bool
