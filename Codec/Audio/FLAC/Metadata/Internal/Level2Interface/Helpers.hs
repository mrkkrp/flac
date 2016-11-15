-- |
-- Module      :  Codec.Audio.FLAC.Metadata.Internal.Level2Interface.Helpers
-- Copyright   :  © 2016 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Wrappers around helpers for working with level 2 FLAC metadata interface.
--
-- The functions from this module are not safe, one only should attempt
-- calling them when 'Metadata' contains metadata of correct type.

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
  , setVorbisVendor
  , getVorbisComment
  , setVorbisComment
  , deleteVorbisComment
  , isVorbisCommentEmpty )
where

import Codec.Audio.FLAC.Metadata.Internal.Types
import Data.ByteString (ByteString)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Word
import Foreign
import Foreign.C.String
import Foreign.C.Types
import qualified Data.ByteString    as B
import qualified Data.Text          as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Foreign  as T

----------------------------------------------------------------------------
-- Stream info

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
getMd5Sum block = do
  md5SumPtr <- c_get_md5sum block
  B.packCStringLen (md5SumPtr, 16)

foreign import ccall unsafe "FLAC__metadata_get_md5sum"
  c_get_md5sum :: Metadata -> IO CString

----------------------------------------------------------------------------
-- Vorbis comment

-- | Get Vorbis vendor.

getVorbisVendor :: Metadata -> IO Text
getVorbisVendor block = alloca $ \sizePtr -> do
  vendorPtr <- c_get_vorbis_vendor block sizePtr
  size      <- fromIntegral <$> peek sizePtr
  T.decodeUtf8 <$> B.packCStringLen (vendorPtr, size)

foreign import ccall unsafe "FLAC__metadata_get_vorbis_vendor"
  c_get_vorbis_vendor :: Metadata -> Ptr Word32 -> IO CString

-- | Set Vorbis vendor.

setVorbisVendor :: Metadata -> Text -> IO Bool
setVorbisVendor block vendor =
  T.withCStringLen vendor $ \(vendorPtr, size) ->
    c_set_vorbis_vendor block vendorPtr (fromIntegral size)

foreign import ccall unsafe "FLAC__metadata_set_vorbis_vendor"
  c_set_vorbis_vendor :: Metadata -> CString -> Word32 -> IO Bool

-- | Get vorbis comment by name.

getVorbisComment :: ByteString -> Metadata -> IO (Maybe Text)
getVorbisComment name block = alloca $ \sizePtr ->
  B.useAsCString name $ \namePtr -> do
    commentPtr  <- c_get_vorbis_comment block namePtr sizePtr
    commentSize <- fromIntegral <$> peek sizePtr
    if commentPtr == nullPtr
      then return Nothing
      else do
        value <- T.drop 1 . T.dropWhile (/= '=') . T.decodeUtf8
          <$> B.packCStringLen (commentPtr, commentSize)
        return (pure value)

foreign import ccall unsafe "FLAC__metadata_get_vorbis_comment"
  c_get_vorbis_comment :: Metadata -> CString -> Ptr Word32 -> IO CString

-- | Set (replace or insert if necessary) a vorbis comment.

setVorbisComment :: ByteString -> Text -> Metadata -> IO Bool
setVorbisComment name value block =
  T.withCStringLen (T.decodeUtf8 name <> "=" <> value) $
    \(commentPtr, commentSize) ->
      c_set_vorbis_comment block commentPtr (fromIntegral commentSize)

foreign import ccall unsafe "FLAC__metadata_set_vorbis_comment"
  c_set_vorbis_comment :: Metadata -> CString -> Word32 -> IO Bool

-- | Delete a vorbis comment by name. If it doesn't exist, nothing will
-- happen.

deleteVorbisComment :: ByteString -> Metadata -> IO Bool
deleteVorbisComment name block =
  B.useAsCString name (c_delete_vorbis_comment block)

foreign import ccall unsafe "FLAC__metadata_delete_vorbis_comment"
  c_delete_vorbis_comment :: Metadata -> CString -> IO Bool

-- | Determine a vorbis comment metadata block can be considered empty.

isVorbisCommentEmpty :: Metadata -> IO Bool
isVorbisCommentEmpty = c_is_vorbis_comment_empty

foreign import ccall unsafe "FLAC__metadata_is_vorbis_comment_empty"
  c_is_vorbis_comment_empty :: Metadata -> IO Bool
