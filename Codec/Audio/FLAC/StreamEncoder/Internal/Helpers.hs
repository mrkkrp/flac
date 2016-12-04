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
import Codec.Audio.FLAC.Util
import Codec.Audio.Wave
import Control.Monad.Catch
import Control.Monad.Reader
import Data.Word (Word64)
import Foreign.C.String
import Foreign.C.Types
import System.Directory
import System.FilePath
import System.IO

-- | Encode given input file, return 'False' in case of failure.

encoderProcessHelper
  :: Encoder           -- ^ 'Encoder' to use
  -> WaveFormat        -- ^ Audio format of input file
  -> Word64            -- ^ Offset of data chunk
  -> Word64            -- ^ Size of data chunk
  -> FilePath          -- ^ Location of input file
  -> FilePath          -- ^ Location of output file
  -> IO Bool
encoderProcessHelper encoder fmt dataOffset dataSize ipath' opath' = do
  ipath <- makeAbsolute ipath'
  opath <- makeAbsolute opath'
  let acquire = fst <$> openBinaryTempFile odir ofile
      cleanup = removeFile
      odir    = takeDirectory opath
      ofile   = takeFileName  opath
  bracketOnError acquire cleanup $ \otemp    ->
    withCString ipath            $ \ipathPtr ->
      withCString otemp          $ \otempPtr -> do
        result <- c_encoder_process_helper
          encoder             -- stream encoder
          (fromEnum' fmt)     -- format of input file
          dataOffset          -- offset of data chunk
          dataSize            -- size of data chunk
          ipathPtr            -- path to input file
          otempPtr            -- path to output temp file
        when result (renameFile otemp opath)
        return result

foreign import ccall unsafe "FLAC__stream_encoder_process_helper"
  c_encoder_process_helper
    :: Encoder -> CUInt -> Word64 -> Word64 -> CString -> CString -> IO Bool
