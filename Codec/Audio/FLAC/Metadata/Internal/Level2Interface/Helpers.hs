-- |
-- Module      :  Codec.Audio.FLAC.Metadata.Internal.Level2Interface.Helpers
-- Copyright   :  © 2016 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Wrappers around helpers written to help work with level 2 FLAC metadata
-- interface. The functions from this module are not safe, one only should
-- attempt calling them when metadata points to metadata of correct type.

{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings        #-}

module Codec.Audio.FLAC.Metadata.Internal.Level2Interface.Helpers
  ( -- * Stream info
    getMinBlockSize
  , getMaxBlockSize
  , getMinFrameSize
  , getMaxFrameSize
  , getSampleRate
  , getChannels
  , getBitsPerSample
  , getTotalSamples
  , getMd5Sum
    -- * Vorbis comment
  , getVorbisVendor
  , getVorbisComment )
where

import Codec.Audio.FLAC.Metadata.Internal.Types
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Word
import Foreign
import Foreign.C.String
import Foreign.C.Types
import qualified Data.ByteString    as B
import qualified Data.Text          as T
import qualified Data.Text.Encoding as T

-- | Get min block size.

getMinBlockSize :: Metadata -> IO Word32
getMinBlockSize = fmap fromIntegral . c_get_min_blocksize

foreign import ccall unsafe "FLAC__metadata_get_min_blocksize"
  c_get_min_blocksize :: Metadata -> IO CUInt

-- | Get max block size.

getMaxBlockSize :: Metadata -> IO Word32
getMaxBlockSize = fmap fromIntegral . c_get_max_blocksize

foreign import ccall unsafe "FLAC__metadata_get_max_blocksize"
  c_get_max_blocksize :: Metadata -> IO CUInt

-- | Get min frame size.

getMinFrameSize :: Metadata -> IO Word32
getMinFrameSize = fmap fromIntegral . c_get_min_framesize

foreign import ccall unsafe "FLAC__metadata_get_min_framesize"
  c_get_min_framesize :: Metadata -> IO Word32

-- | Get max frame size.

getMaxFrameSize :: Metadata -> IO Word32
getMaxFrameSize = fmap fromIntegral . c_get_max_framesize

foreign import ccall unsafe "FLAC__metadata_get_max_framesize"
  c_get_max_framesize :: Metadata -> IO Word32

-- | Get sample rate.

getSampleRate :: Metadata -> IO Word32
getSampleRate = fmap fromIntegral . c_get_sample_rate

foreign import ccall unsafe "FLAC__metadata_get_sample_rate"
  c_get_sample_rate :: Metadata -> IO CUInt

-- | Get number of channels.

getChannels :: Metadata -> IO Word32
getChannels = fmap fromIntegral . c_get_channels

foreign import ccall unsafe "FLAC__metadata_get_channels"
  c_get_channels :: Metadata -> IO CUInt

-- | Get number of bits per sample.

getBitsPerSample :: Metadata -> IO Word32
getBitsPerSample = fmap fromIntegral . c_get_bits_per_sample

foreign import ccall unsafe "FLAC__metadata_get_bits_per_sample"
  c_get_bits_per_sample :: Metadata -> IO CUInt

-- | Get total number of samples.

getTotalSamples :: Metadata -> IO Word64
getTotalSamples = c_get_total_samples

foreign import ccall unsafe "FLAC__metadata_get_total_samples"
  c_get_total_samples :: Metadata -> IO Word64

-- | Get MD5 sum of original audio data.

getMd5Sum :: Metadata -> IO ByteString
getMd5Sum metadata = do
  ptr <- c_get_md5sum metadata
  B.packCStringLen (ptr, 16)

foreign import ccall unsafe "FLAC__metadata_get_md5sum"
  c_get_md5sum :: Metadata -> IO CString

-- | Get Vorbis comment: vendor.

getVorbisVendor :: Metadata -> IO Text
getVorbisVendor metadata = alloca $ \sizePtr -> do
  ptr  <- c_get_vorbis_vendor metadata sizePtr
  size <- fromIntegral <$> peek sizePtr
  T.decodeUtf8 <$> B.packCStringLen (ptr, size)

foreign import ccall unsafe "FLAC__metadata_get_vorbis_vendor"
  c_get_vorbis_vendor :: Metadata -> Ptr Word32 -> IO CString

-- | Get vorbis comment by name.

getVorbisComment :: ByteString -> Metadata -> IO (Maybe Text)
getVorbisComment name metadata = alloca $ \sizePtr ->
  B.useAsCString name $ \cstr -> do
    ptr  <- c_get_vorbis_comment metadata cstr sizePtr
    size <- fromIntegral <$> peek sizePtr
    if ptr == nullPtr
      then return Nothing
      else do
        value <- T.drop 1 . T.dropWhile (/= '=') . T.decodeUtf8
          <$> B.packCStringLen (ptr, size)
        return (pure value)

foreign import ccall unsafe "FLAC__metadata_get_vorbis_comment"
  c_get_vorbis_comment :: Metadata -> CString -> Ptr Word32 -> IO CString
