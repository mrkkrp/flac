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
{-# LANGUAGE RecordWildCards          #-}

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
    -- * Application
  , getApplicationId
  , getApplicationData
  , setApplicationId
  , setApplicationData
    -- * Seek table
  , getSeekPoints
  , setSeekPoints
    -- * Vorbis comment
  , getVorbisVendor
  , setVorbisVendor
  , getVorbisComment
  , setVorbisComment
  , deleteVorbisComment
  , isVorbisCommentEmpty
    -- * Picture
  , getPictureType
  , getPictureData
  , setPictureType
  , setPictureData )
where

import Codec.Audio.FLAC.Metadata.Internal.Object
import Codec.Audio.FLAC.Metadata.Internal.Types
import Codec.Audio.FLAC.Util
import Control.Monad
import Control.Monad.Catch
import Data.Bool (bool)
import Data.ByteString (ByteString)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Vector (Vector, (!))
import Data.Word
import Foreign
import Foreign.C.String
import Foreign.C.Types
import qualified Data.ByteString     as B
import qualified Data.Text           as T
import qualified Data.Text.Encoding  as T
import qualified Data.Text.Foreign   as T
import qualified Data.Vector         as V
import qualified Data.Vector.Mutable as VM

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
-- Application

-- | Get application id from given 'Metadata' block.

getApplicationId :: Metadata -> IO ByteString
getApplicationId block = do
  idPtr <- c_get_application_id block
  B.packCStringLen (idPtr, 4)

foreign import ccall unsafe "FLAC__metadata_get_application_id"
  c_get_application_id :: Metadata -> IO CString

-- | Get data from given application metadata block.

getApplicationData :: Metadata -> IO ByteString
getApplicationData block = alloca $ \sizePtr -> do
  dataPtr <- c_get_application_data block sizePtr
  size    <- fromIntegral <$> peek sizePtr
  B.packCStringLen (dataPtr, size)

foreign import ccall unsafe "FLAC__metadata_get_application_data"
  c_get_application_data :: Metadata -> Ptr CUInt -> IO CString

-- | Set application id for given metadata block.

setApplicationId :: Metadata -> ByteString -> IO ()
setApplicationId block appId =
  B.useAsCString appId' (c_set_application_id block)
  where
    appId' = B.take 4 (appId <> B.replicate 4 0x00)

foreign import ccall unsafe "FLAC__metadata_set_application_id"
  c_set_application_id :: Metadata -> CString -> IO ()

-- | Set application data for given metadata block.

setApplicationData :: Metadata -> ByteString -> IO Bool
setApplicationData block data' =
  B.useAsCString data' $ \dataPtr -> do
    let size = fromIntegral (B.length data')
    c_set_application_data block dataPtr size

foreign import ccall unsafe "FLAC__metadata_set_application_data"
  c_set_application_data :: Metadata -> CString -> CUInt -> IO Bool

----------------------------------------------------------------------------
-- Seek table

-- | Get seek table as a 'Vector' of 'SeekPoint's.

getSeekPoints :: Metadata -> IO (Vector SeekPoint)
getSeekPoints block = do
  size <- fromIntegral <$> c_get_seek_points_num block
  v    <- VM.new size
  let go n =
        when (n < size) $ do
          c_get_seek_point block (fromIntegral n) >>= peek >>= VM.write v n
          go (n + 1)
  go 0
  V.unsafeFreeze v

foreign import ccall unsafe "FLAC__metadata_get_seek_points_num"
  c_get_seek_points_num :: Metadata -> IO CUInt

foreign import ccall unsafe "FLAC__metadata_get_seek_point"
  c_get_seek_point :: Metadata -> CUInt -> IO (Ptr SeekPoint)

-- | Set seek table represented by a given 'Vector' of 'SeekPoint's. Return
-- 'False' in case of trouble.

setSeekPoints :: Metadata -> Vector SeekPoint -> IO Bool
setSeekPoints block seekPoints = do
  let size = fromIntegral (V.length seekPoints)
  res <- objectSeektableResizePoints block size
  if res
    then
      let go n =
            if n < size
              then do
                let SeekPoint {..} = seekPoints ! fromIntegral n
                c_set_seek_point block (fromIntegral n)
                  seekPointSampleNumber
                  seekPointStreamOffset
                  seekPointFrameSamples
                go (n + 1)
              else do
                legal <- objectSeektableIsLegal block
                unless legal $
                  throwM FlacMetaIncorrectSeekTable
      in go 0 >> return True
    else return False

foreign import ccall unsafe "FLAC__metadata_set_seek_point"
  c_set_seek_point :: Metadata -> CUInt -> Word64 -> Word64 -> Word32 -> IO ()

----------------------------------------------------------------------------
-- Vorbis comment

-- | Get Vorbis vendor.

getVorbisVendor :: Metadata -> IO Text
getVorbisVendor block = alloca $ \sizePtr -> do
  vendorPtr <- c_get_vorbis_vendor block sizePtr
  size      <- fromIntegral <$> peek sizePtr
  T.peekCStringLen (vendorPtr, size)

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

----------------------------------------------------------------------------
-- Picture

-- | Get type of picture assuming that given 'Metadata' block is a
-- 'PictureBolck'.

getPictureType :: Metadata -> IO PictureType
getPictureType = fmap toEnum' . c_get_picture_type

foreign import ccall unsafe "FLAC__metadata_get_picture_type"
  c_get_picture_type :: Metadata -> IO CUInt

-- | Get picture data from given 'Metadata' block.

getPictureData :: Metadata -> IO PictureData
getPictureData block = do
  pictureMimeType    <- c_get_picture_mime_type   block >>= peekCStringText
  pictureDescription <- c_get_picture_description block >>= peekCStringText
  pictureWidth       <- c_get_picture_width       block
  pictureHeight      <- c_get_picture_height      block
  pictureDepth       <- c_get_picture_depth       block
  pictureColors      <- c_get_picture_colors      block
  pictureData        <- alloca $ \dataSizePtr -> do
    dataPtr  <- c_get_picture_data block dataSizePtr
    dataSize <- fromIntegral <$> peek dataSizePtr
    B.packCStringLen (dataPtr, dataSize)
  return PictureData {..}

foreign import ccall unsafe "FLAC__metadata_get_picture_mime_type"
  c_get_picture_mime_type :: Metadata -> IO CString

foreign import ccall unsafe "FLAC__metadata_get_picture_description"
  c_get_picture_description :: Metadata -> IO CString

foreign import ccall unsafe "FLAC__metadata_get_picture_width"
  c_get_picture_width :: Metadata -> IO Word32

foreign import ccall unsafe "FLAC__metadata_get_picture_height"
  c_get_picture_height :: Metadata -> IO Word32

foreign import ccall unsafe "FLAC__metadata_get_picture_depth"
  c_get_picture_depth :: Metadata -> IO Word32

foreign import ccall unsafe "FLAC__metadata_get_picture_colors"
  c_get_picture_colors :: Metadata -> IO Word32

foreign import ccall unsafe "FLAC__metadata_get_picture_data"
  c_get_picture_data :: Metadata -> Ptr Word32 -> IO CString

-- | Set 'PictureType' to given 'Metadata' block that should be a
-- 'PictureBlock'.

setPictureType :: Metadata -> PictureType -> IO ()
setPictureType block pictureType =
  c_set_picture_type block (fromEnum' pictureType)

foreign import ccall unsafe "FLAC__metadata_set_picture_type"
  c_set_picture_type :: Metadata -> CUInt -> IO ()

-- | Set 'PictureData' in given 'Metadata' block of type 'PictureBlock'.

setPictureData :: Metadata -> PictureData -> IO Bool
setPictureData block PictureData {..} = do
  c_set_picture_width  block pictureWidth
  c_set_picture_height block pictureHeight
  c_set_picture_depth  block pictureDepth
  c_set_picture_colors block pictureColors
  shortcutFalse
    [ objectPictureSetMimeType    block pictureMimeType
    , objectPictureSetDescription block pictureDescription
    , objectPictureSetData        block pictureData ]

foreign import ccall unsafe "FLAC__metadata_set_picture_width"
  c_set_picture_width :: Metadata -> Word32 -> IO ()

foreign import ccall unsafe "FLAC__metadata_set_picture_height"
  c_set_picture_height :: Metadata -> Word32 -> IO ()

foreign import ccall unsafe "FLAC__metadata_set_picture_depth"
  c_set_picture_depth :: Metadata -> Word32 -> IO ()

foreign import ccall unsafe "FLAC__metadata_set_picture_colors"
  c_set_picture_colors :: Metadata -> Word32 -> IO ()

-- | Execute a collection of actions that return 'False' on failure. As soon
-- as failure is reported, stop the execution and return 'False'. Return
-- 'True' in case of success.

shortcutFalse :: [IO Bool] -> IO Bool
shortcutFalse []     = return True
shortcutFalse (m:ms) = m >>= bool (return False) (shortcutFalse ms)
