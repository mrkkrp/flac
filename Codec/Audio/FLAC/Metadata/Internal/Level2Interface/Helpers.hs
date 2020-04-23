{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Module      :  Codec.Audio.FLAC.Metadata.Internal.Level2Interface.Helpers
-- Copyright   :  © 2016–present Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Wrappers around helpers for working with level 2 FLAC metadata interface.
--
-- The functions from this module are not safe, one only should attempt
-- calling them when 'Metadata' contains metadata of correct type.
module Codec.Audio.FLAC.Metadata.Internal.Level2Interface.Helpers
  ( -- * Stream info
    getMinBlockSize,
    getMaxBlockSize,
    getMinFrameSize,
    getMaxFrameSize,
    getSampleRate,
    getChannels,
    getBitsPerSample,
    getTotalSamples,
    getMd5Sum,

    -- * Application
    getApplicationId,
    getApplicationData,
    setApplicationId,
    setApplicationData,

    -- * Seek table
    getSeekPoints,
    setSeekPoints,

    -- * Vorbis comment
    getVorbisVendor,
    setVorbisVendor,
    getVorbisComment,
    setVorbisComment,
    deleteVorbisComment,
    isVorbisCommentEmpty,

    -- * CUE sheet
    getCueSheetData,
    setCueSheetData,

    -- * Picture
    getPictureType,
    getPictureData,
    setPictureType,
    setPictureData,
  )
where

import Codec.Audio.FLAC.Metadata.Internal.Object
import Codec.Audio.FLAC.Metadata.Internal.Types
import Codec.Audio.FLAC.Util
import Control.Monad
import Control.Monad.Catch
import Data.Bool (bool)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.List (uncons)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Foreign as T
import Data.Vector ((!), Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import Data.Word
import Foreign
import Foreign.C.String
import Foreign.C.Types

#if !MIN_VERSION_base(4,13,0)
import Data.Semigroup ((<>))
#endif

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
getApplicationId :: Metadata -> IO ApplicationId
getApplicationId block = do
  idPtr <- c_get_application_id block
  mkApplicationId <$> B.packCStringLen (idPtr, 4)

foreign import ccall unsafe "FLAC__metadata_get_application_id"
  c_get_application_id :: Metadata -> IO CString

-- | Get data from given application metadata block.
getApplicationData :: Metadata -> IO ByteString
getApplicationData block = alloca $ \sizePtr -> do
  dataPtr <- c_get_application_data block sizePtr
  size <- fromIntegral <$> peek sizePtr
  B.packCStringLen (dataPtr, size)

foreign import ccall unsafe "FLAC__metadata_get_application_data"
  c_get_application_data :: Metadata -> Ptr CUInt -> IO CString

-- | Set application id for given metadata block.
setApplicationId :: Metadata -> ApplicationId -> IO ()
setApplicationId block appId =
  B.useAsCString (unApplicationId appId) (c_set_application_id block)

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
  v <- VM.new size
  let go n =
        when (n < size) $ do
          ptr <- c_get_seek_point block (fromIntegral n)
          seekPointSampleNumber <- peekByteOff ptr 0
          seekPointStreamOffset <- peekByteOff ptr 8
          seekPointFrameSamples <- peekByteOff ptr 16
          VM.write v n SeekPoint {..}
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
                c_set_seek_point
                  block
                  (fromIntegral n)
                  seekPointSampleNumber
                  seekPointStreamOffset
                  seekPointFrameSamples
                go (n + 1)
              else do
                legal <- objectSeektableIsLegal block
                unless legal $
                  throwM MetaInvalidSeekTable
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
  size <- fromIntegral <$> peek sizePtr
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
    commentPtr <- c_get_vorbis_comment block namePtr sizePtr
    commentSize <- fromIntegral <$> peek sizePtr
    if commentPtr == nullPtr
      then return Nothing
      else
        Just . T.drop 1 . T.dropWhile (/= '=') . T.decodeUtf8
          <$> B.packCStringLen (commentPtr, commentSize)

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
-- CUE sheet

-- | Get CUE sheet from 'Metadata' block assuming that it's a
-- 'CueSheetBlock'.
getCueSheetData :: Metadata -> IO CueSheetData
getCueSheetData block = do
  cueCatalog <- c_get_cue_sheet_mcn block >>= B.packCString
  cueLeadIn <- c_get_cue_sheet_lead_in block
  cueIsCd <- c_get_cue_sheet_is_cd block
  numTracks <- c_get_cue_sheet_num_tracks block
  (cueTracks, cueLeadOutTrack) <-
    case numTracks of
      0 ->
        -- NOTE Should probably never happen unless FLAC file is invalid
        -- with respect to the spec.
        throwM (MetaInvalidCueSheet "Cannot read CUE sheet without tracks")
      1 -> ([],) <$> getCueSheetTrack block 0
      _ -> do
        ts <- mapM (getCueSheetTrack block) [0 .. numTracks - 2]
        t' <- getCueSheetTrack block (numTracks - 1)
        return (ts, t')
  return CueSheetData {..}

foreign import ccall unsafe "FLAC__metadata_get_cue_sheet_mcn"
  c_get_cue_sheet_mcn :: Metadata -> IO CString

foreign import ccall unsafe "FLAC__metadata_get_cue_sheet_lead_in"
  c_get_cue_sheet_lead_in :: Metadata -> IO Word64

foreign import ccall unsafe "FLAC__metadata_get_cue_sheet_is_cd"
  c_get_cue_sheet_is_cd :: Metadata -> IO Bool

foreign import ccall unsafe "FLAC__metadata_get_cue_sheet_num_tracks"
  c_get_cue_sheet_num_tracks :: Metadata -> IO Word8

-- | Peek a single 'CueSheetTrack' at given index.
getCueSheetTrack :: Metadata -> Word8 -> IO CueTrack
getCueSheetTrack block n = do
  cueTrackOffset <- c_get_cue_sheet_track_offset block n
  cueTrackIsrc <- c_get_cue_sheet_track_isrc block n >>= B.packCString
  cueTrackAudio <- c_get_cue_sheet_track_audio block n
  cueTrackPreEmphasis <- c_get_cue_sheet_track_preemphasis block n
  numIndices <- c_get_cue_sheet_track_num_indices block n
  (cueTrackPregapIndex, cueTrackIndices) <-
    if numIndices == 0
      then throwM (MetaInvalidCueSheet "Cannot read CUE track without indices")
      else do
        hasPregap <- c_get_cue_sheet_track_has_pregap_index block n
        let pregapOne :: Num a => a
            pregapOne = if hasPregap then 1 else 0
            range =
              if numIndices > pregapOne
                then [pregapOne .. numIndices - 1]
                else []
        pregapIndex <-
          if hasPregap
            then Just <$> c_get_cue_sheet_track_index block n 0
            else return Nothing
        trackIndices <-
          mapM
            (c_get_cue_sheet_track_index block n)
            (NE.fromList range)
        return (pregapIndex, trackIndices)
  return CueTrack {..}

foreign import ccall unsafe "FLAC__metadata_get_cue_sheet_track_offset"
  c_get_cue_sheet_track_offset :: Metadata -> Word8 -> IO Word64

foreign import ccall unsafe "FLAC__metadata_get_cue_sheet_track_isrc"
  c_get_cue_sheet_track_isrc :: Metadata -> Word8 -> IO CString

foreign import ccall unsafe "FLAC__metadata_get_cue_sheet_track_audio"
  c_get_cue_sheet_track_audio :: Metadata -> Word8 -> IO Bool

foreign import ccall unsafe "FLAC__metadata_get_cue_sheet_track_preemphasis"
  c_get_cue_sheet_track_preemphasis :: Metadata -> Word8 -> IO Bool

foreign import ccall unsafe "FLAC__metadata_get_cue_sheet_track_num_indices"
  c_get_cue_sheet_track_num_indices :: Metadata -> Word8 -> IO Word8

foreign import ccall unsafe "FLAC__metadata_get_cue_sheet_track_has_pregap_index"
  c_get_cue_sheet_track_has_pregap_index :: Metadata -> Word8 -> IO Bool

foreign import ccall unsafe "FLAC__metadata_get_cue_sheet_track_index"
  c_get_cue_sheet_track_index :: Metadata -> Word8 -> Word8 -> IO Word64

-- | Set 'CueSheetData' in given 'Metadata' block of type 'CueSheetBlock'.
setCueSheetData :: Metadata -> CueSheetData -> IO Bool
setCueSheetData block CueSheetData {..} = do
  B.useAsCStringLen cueCatalog $ \(mcnPtr, mcnSize) ->
    c_set_cue_sheet_mcn block mcnPtr (fromIntegral mcnSize)
  c_set_cue_sheet_lead_in block cueLeadIn
  c_set_cue_sheet_is_cd block cueIsCd
  let numTracks = fromIntegral (length cueTracks + 1)
  res <- objectCueSheetResizeTracks block numTracks
  goodOutcome <-
    if res
      then
        let go ts =
              case uncons ts of
                Nothing ->
                  setCueSheetTrack block cueLeadOutTrack (numTracks - 1) 170
                Just ((t, n), ts') -> do
                  res' <- setCueSheetTrack block t n (n + 1)
                  if res'
                    then go ts'
                    else return False
         in go (zip cueTracks [0 ..])
      else return False
  when goodOutcome $ do
    res' <- objectCueSheetIsLegal block cueIsCd
    case res' of
      Nothing -> return ()
      Just msg -> throwM (MetaInvalidCueSheet msg)
  return goodOutcome

foreign import ccall unsafe "FLAC__metadata_set_cue_sheet_mcn"
  c_set_cue_sheet_mcn :: Metadata -> CString -> CUInt -> IO ()

foreign import ccall unsafe "FLAC__metadata_set_cue_sheet_lead_in"
  c_set_cue_sheet_lead_in :: Metadata -> Word64 -> IO ()

foreign import ccall unsafe "FLAC__metadata_set_cue_sheet_is_cd"
  c_set_cue_sheet_is_cd :: Metadata -> Bool -> IO ()

-- | Poke a 'CueTrack' at a specified index.
setCueSheetTrack :: Metadata -> CueTrack -> Word8 -> Word8 -> IO Bool
setCueSheetTrack block CueTrack {..} n n' = do
  c_set_cue_sheet_track_offset block n cueTrackOffset
  c_set_cue_sheet_track_number block n n'
  B.useAsCStringLen cueTrackIsrc $ \(isrcPtr, isrcSize) ->
    c_set_cue_sheet_track_isrc block n isrcPtr (fromIntegral isrcSize)
  c_set_cue_sheet_track_audio block n cueTrackAudio
  c_set_cue_sheet_track_pre_emphasis block n cueTrackPreEmphasis
  let pregapOne :: Num a => a
      pregapOne = if isJust cueTrackPregapIndex then 1 else 0
      numIndices = fromIntegral (NE.length cueTrackIndices + pregapOne)
  goodOutcome <- objectCueSheetTrackResizeIndices block n numIndices
  when goodOutcome $ do
    forM_ cueTrackPregapIndex $ \offset ->
      c_set_cue_sheet_track_index block n 0 0 offset
    let range = zip [pregapOne ..] [1 ..]
    forM_ (NE.zip cueTrackIndices (NE.fromList range)) $ \(offset, (i, i')) ->
      c_set_cue_sheet_track_index block n i i' offset
  return goodOutcome

foreign import ccall unsafe "FLAC__metadata_set_cue_sheet_track_offset"
  c_set_cue_sheet_track_offset :: Metadata -> Word8 -> Word64 -> IO ()

foreign import ccall unsafe "FLAC__metadata_set_cue_sheet_track_number"
  c_set_cue_sheet_track_number :: Metadata -> Word8 -> Word8 -> IO ()

foreign import ccall unsafe "FLAC__metadata_set_cue_sheet_track_isrc"
  c_set_cue_sheet_track_isrc :: Metadata -> Word8 -> CString -> CUInt -> IO ()

foreign import ccall unsafe "FLAC__metadata_set_cue_sheet_track_audio"
  c_set_cue_sheet_track_audio :: Metadata -> Word8 -> Bool -> IO ()

foreign import ccall unsafe "FLAC__metadata_set_cue_sheet_track_pre_emphasis"
  c_set_cue_sheet_track_pre_emphasis :: Metadata -> Word8 -> Bool -> IO ()

foreign import ccall unsafe "FLAC__metadata_set_cue_sheet_track_index"
  c_set_cue_sheet_track_index :: Metadata -> Word8 -> Word8 -> Word8 -> Word64 -> IO ()

----------------------------------------------------------------------------
-- Picture

-- | Get type of picture assuming that given 'Metadata' block is a
-- 'PictureBlock'.
getPictureType :: Metadata -> IO PictureType
getPictureType = fmap toEnum' . c_get_picture_type

foreign import ccall unsafe "FLAC__metadata_get_picture_type"
  c_get_picture_type :: Metadata -> IO CUInt

-- | Get picture data from a given 'Metadata' block.
getPictureData :: Metadata -> IO PictureData
getPictureData block = do
  pictureMimeType <- c_get_picture_mime_type block >>= peekCStringText
  pictureDescription <- c_get_picture_description block >>= peekCStringText
  pictureWidth <- c_get_picture_width block
  pictureHeight <- c_get_picture_height block
  pictureDepth <- c_get_picture_depth block
  pictureColors <- c_get_picture_colors block
  pictureData <- alloca $ \dataSizePtr -> do
    dataPtr <- c_get_picture_data block dataSizePtr
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

-- | Set 'PictureType' to a given 'Metadata' block that should be a
-- 'PictureBlock'.
setPictureType :: Metadata -> PictureType -> IO ()
setPictureType block pictureType =
  c_set_picture_type block (fromEnum' pictureType)

foreign import ccall unsafe "FLAC__metadata_set_picture_type"
  c_set_picture_type :: Metadata -> CUInt -> IO ()

-- | Set 'PictureData' in given 'Metadata' block of type 'PictureBlock'.
setPictureData :: Metadata -> PictureData -> IO Bool
setPictureData block PictureData {..} = do
  c_set_picture_width block pictureWidth
  c_set_picture_height block pictureHeight
  c_set_picture_depth block pictureDepth
  c_set_picture_colors block pictureColors
  goodOutcome <-
    shortcutFalse
      [ objectPictureSetMimeType block pictureMimeType,
        objectPictureSetDescription block pictureDescription,
        objectPictureSetData block pictureData
      ]
  when goodOutcome $ do
    res <- objectPictureIsLegal block
    case res of
      Nothing -> return ()
      Just msg -> throwM (MetaInvalidPicture msg)
  return goodOutcome

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
-- 'True' in the case of success.
shortcutFalse :: [IO Bool] -> IO Bool
shortcutFalse [] = return True
shortcutFalse (m : ms) = m >>= bool (return False) (shortcutFalse ms)
