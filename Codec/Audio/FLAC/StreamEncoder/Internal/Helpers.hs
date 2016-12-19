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
{-# LANGUAGE OverloadedStrings        #-}

module Codec.Audio.FLAC.StreamEncoder.Internal.Helpers
  ( encoderProcessHelper )
where

import Codec.Audio.FLAC.StreamEncoder.Internal.Types
import Data.Word (Word64)
import Foreign.C.String

-- | Encode given input file, return 'False' in case of failure.

encoderProcessHelper
  :: Encoder           -- ^ 'Encoder' to use
  -> Word64            -- ^ Offset of data chunk
  -> Word64            -- ^ Size of data chunk
  -> FilePath          -- ^ Location of input file (normalized)
  -> IO Bool
encoderProcessHelper encoder dataOffset dataSize ipath =
  withCString ipath $ \ipathPtr ->
    c_encoder_process_helper
      encoder             -- stream encoder
      dataOffset          -- offset of data chunk
      dataSize            -- size of data chunk
      ipathPtr            -- path to input file

foreign import ccall unsafe "FLAC__stream_encoder_process_helper"
  c_encoder_process_helper :: Encoder -> Word64 -> Word64 -> CString -> IO Bool
