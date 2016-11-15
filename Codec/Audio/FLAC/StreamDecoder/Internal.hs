-- |
-- Module      :  Codec.Audio.FLAC.StreamDecoder.Internal
-- Copyright   :  © 2016 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Low-level Haskell wrapper around FLAC stream decoder API.

{-# LANGUAGE ForeignFunctionInterface #-}

module Codec.Audio.FLAC.StreamDecoder.Internal
  ( decoderNew
  , decoderDelete
  , decoderSetMd5Checking
  -- TODO FLAC__stream_decoder_set_metadata_respond
  -- TODO FLAC__stream_decoder_set_metadata_respond_application
  -- TODO FLAC__stream_decoder_set_metadata_respond_all
  -- TODO FLAC__stream_decoder_set_metadata_ignore
  -- TODO FLAC__stream_decoder_set_metadata_ignore_application
  -- TODO FLAC__stream_decoder_set_metadata_ignore_all
  , decoderInitFile
  , decoderFinish
  )
where

import Codec.Audio.FLAC.Util
import Data.Void
import Foreign
import Foreign.C.String
import Foreign.C.Types

newtype Decoder = Decoder (Ptr Void)

data DecoderInitStatus
  = DecoderInitStatusOK
    -- ^ Initialization was successful.
  | DecoderInitStatusUnsupportedContainer
    -- ^ The library was not compiled with support for the given container
    -- format.
  | DecoderInitStatusInvalidCallbacks
    -- ^ A required callback was not supplied.
  | DecoderInitStatusMemoryAllocationError
    -- ^ An error occurred allocating memory.
  | DecoderInitStatusErrorOpeningFile
    -- ^ fopen() failed.
  | DecoderInitStatusAlreadyInitialized
    -- ^ FLAC__stream_decoder_init_*() was called when the decoder was
    -- already initialized, usually because FLAC__stream_decoder_finish()
    -- was not called
  deriving (Show, Read, Eq, Ord, Bounded, Enum)

data DecoderState
  = DecoderStateSearchForMetadata
    -- ^ The decoder is ready to search for metadata.
  | DecoderStateReadMetadata
    -- ^ The decoder is ready to or is in the process of reading metadata.
  | DecoderStateSearchForFrameSync
    -- ^ The decoder is ready to or is in the process of searching for the
    -- frame sync code.
  | DecoderStateReadFrame
    -- ^ The decoder is ready to or is in the process of reading a frame.
  | DecoderStateEndOfStream
    -- ^ The decoder has reached the end of the stream.
  | DecoderStateOggError
    -- ^ An error occurred in the underlying Ogg layer.
  | DecoderStateSeekError
    -- ^ An error occurred while seeking. The decoder must be flushed or
    -- reset before decoding can continue.
  | DecoderStateAborted
    -- ^ The decoder was aborted by the read callback.
  | DecoderStateMemoryAllocationError
    -- ^ An error occurred allocating memory. The decoder is in an invalid
    -- state and can no longer be used.
  | DecoderStateUnititialized
    -- ^ The decoder is in the uninitialized state.
  deriving (Show, Read, Eq, Ord, Bounded, Enum)

-- | Create a new stream decoder instance with default settings. In the case
-- of memory allocation problem 'Nothing' is returned.

decoderNew :: IO (Maybe Decoder)
decoderNew = maybePtr <$> c_decoder_new

foreign import ccall unsafe "FLAC__stream_decoder_new"
  c_decoder_new :: IO Decoder

-- | Free a decoder instance.

decoderDelete :: Decoder -> IO ()
decoderDelete = c_decoder_delete

foreign import ccall unsafe "FLAC__stream_decoder_delete"
  c_decoder_delete :: Decoder -> IO ()

-- | Set MD5 signature checking. If 'True' the decoder will compute the MD5
-- signature of the unencoded audio data while decoding and compare it to
-- the signature from the stream info block

decoderSetMd5Checking :: Decoder -> Bool -> IO Bool
decoderSetMd5Checking = c_decoder_set_md5_checking

foreign import ccall unsafe "FLAC__stream_decoder_set_md5_checking"
  c_decoder_set_md5_checking :: Decoder -> Bool -> IO Bool

-- | Initialize the decoder instance to decode native FLAC files.

decoderInitFile :: Decoder -> FilePath -> IO DecoderInitStatus
decoderInitFile decoder path =
  withCString path $ \cstr ->
    toEnum' <$> c_decoder_init_file decoder cstr

foreign import ccall unsafe "FLAC__stream_decoder_init_file"
  c_decoder_init_file :: Decoder -> CString -> IO CUInt

-- | Finish the decoding process and release resources (also resets decoder
-- and its settings). Return 'False' in case of trouble.

decoderFinish :: Decoder -> IO Bool
decoderFinish = c_decoder_finish

foreign import ccall unsafe "FLAC__stream_decoder_finish"
  c_decoder_finish :: Decoder -> IO Bool
