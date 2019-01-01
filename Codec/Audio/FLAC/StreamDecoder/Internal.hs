-- |
-- Module      :  Codec.Audio.FLAC.StreamDecoder.Internal
-- Copyright   :  © 2016–2019 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Low-level Haskell wrapper around FLAC stream decoder API.

{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LambdaCase               #-}

module Codec.Audio.FLAC.StreamDecoder.Internal
  ( withDecoder
  , decoderSetMd5Checking
  , decoderGetState
  , decoderGetBlockSize
  , decoderProcessSingle
  , decoderProcessUntilEndOfMetadata
  , decoderFinish )
where

import Codec.Audio.FLAC.StreamDecoder.Internal.Types
import Codec.Audio.FLAC.Util
import Control.Monad.Catch
import Data.Word
import Foreign.C.Types

-- | Create and use a 'Decoder'. The decoder is guaranteed to be freed even
-- in the case of exception.
--
-- If memory for the encoder cannot be allocated, corresponding
-- 'DecoderException' is raised.

withDecoder :: (Decoder -> IO a) -> IO a
withDecoder f = bracket decoderNew (mapM_ decoderDelete) $ \case
  Nothing -> throwM
    (DecoderFailed DecoderStateMemoryAllocationError)
  Just x -> f x

-- | Create a new stream decoder instance with the default settings. In the
-- case of memory allocation problem 'Nothing' is returned.

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
-- the signature from the stream info block.

decoderSetMd5Checking :: Decoder -> Bool -> IO Bool
decoderSetMd5Checking = c_decoder_set_md5_checking

foreign import ccall unsafe "FLAC__stream_decoder_set_md5_checking"
  c_decoder_set_md5_checking :: Decoder -> Bool -> IO Bool

-- | Get the current decoder state.

decoderGetState :: Decoder -> IO DecoderState
decoderGetState = fmap toEnum' . c_decoder_get_state

foreign import ccall unsafe "FLAC__stream_decoder_get_state"
  c_decoder_get_state :: Decoder -> IO CUInt

-- | Get frame size as a number of inter-channel samples of last decoded
-- frame.

decoderGetBlockSize :: Decoder -> IO Word32
decoderGetBlockSize = fmap fromIntegral . c_decoder_get_blocksize

foreign import ccall unsafe "FLAC__stream_decoder_get_blocksize"
  c_decoder_get_blocksize :: Decoder -> IO CUInt

-- | Process one audio frame. Return 'False' on failure.

decoderProcessSingle :: Decoder -> IO Bool
decoderProcessSingle = c_decoder_process_single

foreign import ccall unsafe "FLAC__stream_decoder_process_single"
  c_decoder_process_single :: Decoder -> IO Bool

-- | Decode until the end of the metadata. We use this to skip to the audio
-- stream.

decoderProcessUntilEndOfMetadata :: Decoder -> IO Bool
decoderProcessUntilEndOfMetadata = c_decoder_process_until_end_of_metadata

foreign import ccall unsafe "FLAC__stream_decoder_process_until_end_of_metadata"
  c_decoder_process_until_end_of_metadata :: Decoder -> IO Bool

-- | Finish the decoding process and release resources (also resets decoder
-- and its settings). Return 'False' in the case of trouble.

decoderFinish :: Decoder -> IO Bool
decoderFinish = c_decoder_finish

foreign import ccall unsafe "FLAC__stream_decoder_finish"
  c_decoder_finish :: Decoder -> IO Bool
