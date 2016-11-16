-- |
-- Module      :  Codec.Audio.FLAC.StreamEncoder
-- Copyright   :  © 2016 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Interface to stream encoder.

{-# LANGUAGE RecordWildCards #-}

module Codec.Audio.FLAC.StreamEncoder
  ( EncoderSettings (..)
  , EncoderInitStatus (..)
  , EncoderState (..)
  , encodeFlac )
where

import Codec.Audio.FLAC.StreamEncoder.Internal
import Codec.Audio.FLAC.StreamEncoder.Internal.Helpers
import Codec.Audio.FLAC.StreamEncoder.Internal.Types
import Control.Exception (bracket)
import Control.Monad.Except
import Data.Bool (bool)
import Data.Default.Class
import Data.Word

-- | Parameters of stream encoder and input to encode. Note that the
-- 'encoderCompression' parameter influences a number of other parameters on
-- its own as specified here
-- <https://xiph.org/flac/api/group__flac__stream__encoder.html#gae49cf32f5256cb47eecd33779493ac85>.
-- The parameters that it sets automatically are wrapped in 'Maybe's, so you
-- can choose to use the value that is set by 'encoderCompression'
-- specifying 'Nothing' (default), or use something specific by passing a
-- value inside 'Just'. Thorough understanding of the FLAC format is
-- necessary to achieve good results, though.

data EncoderSettings = EncoderSettings
  { encoderChannels      :: !Word32 -- ^ Number of channels, default is 2
  , encoderBitsPerSample :: !Word32 -- ^ Sample resolution in bits, default is 16
  , encoderSampleRate    :: !Word32 -- ^ Sample rate in Hz, default is 44100
  , encoderCompression   :: !Word32 -- ^ Compression level [0..8], default is 5
  , encoderBlockSize     :: !Word32 -- ^ Block size, default is 0
  , encoderVerify        :: !Bool   -- ^ Verify result (slower), default is 'False'
  , encoderDoMidSideStereo :: !(Maybe Bool)
    -- ^ Enable mid-side encoding on stereo input. The number of channels
    -- must be 2 for this to have any effect. Default value: 'Nothing'.
  }

instance Default EncoderSettings where
  def = EncoderSettings
    { encoderChannels      = 2
    , encoderBitsPerSample = 16
    , encoderSampleRate    = 44100
    , encoderCompression   = 5
    , encoderBlockSize     = 0
    , encoderVerify        = False
    , encoderDoMidSideStereo = Nothing
    }

-- https://xiph.org/flac/api/group__flac__stream__encoder.html
-- Work with files, encode directly from a file.

data EncoderFailure
  = EncoderInitFailed EncoderInitStatus
  | EncoderFailed     EncoderState

type Inner a = ExceptT EncoderFailure IO a

-- | Encode a file (?) to native FLAC.

encodeFlac
  :: MonadIO m
  => EncoderSettings   -- ^ Encoder settings
  -> FilePath          -- ^ File to encode
  -> FilePath          -- ^ Where to save the resulting FLAC file
  -> m (Either EncoderFailure ())
encodeFlac EncoderSettings {..} source result =
  liftIO (bracket acquire release action)
  where
    acquire = encoderNew
    release = mapM_ encoderDelete
    action mencoder =
      case mencoder of
        Just encoder -> runExceptT $ do
          liftInit (encoderSetChannels      encoder encoderChannels)
          liftInit (encoderSetBitsPerSample encoder encoderBitsPerSample)
          liftInit (encoderSetSampleRate    encoder encoderSampleRate)
          liftInit (encoderSetCompression   encoder encoderCompression)
          liftInit (encoderSetBlockSize     encoder encoderBlockSize)
          liftInit (encoderSetVerify        encoder encoderVerify)
          forM_ encoderDoMidSideStereo (liftInit . encoderSetVerify encoder)
          -- TODO add more parameters here later
          liftInit' (encoderInitFile encoder result)
          liftBool encoder (encoderProcessWav encoder source)
          liftBool encoder (encoderFinish encoder)
        Nothing -> (return . Left . EncoderFailed)
          EnocderStateMemoryAllocationError

----------------------------------------------------------------------------
-- Helpers

-- | Lift an initializing action that returns 'False' on failure into
-- 'Inner' monad taking care of error reporting. In case of trouble,
-- 'EncoderInitFailed' 'EncoderInitStatusAlreadyInitialized' is thrown.

liftInit :: IO Bool -> Inner ()
liftInit m = liftIO m >>= bool throw (return ())
  where
    throw = throwError (EncoderInitFailed EncoderInitStatusAlreadyInitialized)

-- | Lift an action that returns 'EncoderInitStatus' taking care of error
-- reporting.

liftInit' :: IO EncoderInitStatus -> Inner ()
liftInit' m = do
  res <- liftIO m
  case res of
    EncoderInitStatusOK -> return ()
    status -> throwError (EncoderInitFailed status)

-- | Lift an action that returns 'False' on failure into 'Inner' monad
-- taking care of error reporting.

liftBool :: Encoder -> IO Bool -> Inner ()
liftBool encoder m = liftIO m >>= bool (throwStatus encoder) (return ())

-- | Get 'EncoderState' from given 'Encoder' and throw it immediately.

throwStatus :: Encoder -> Inner a
throwStatus = liftIO . encoderGetState >=> throwError . EncoderFailed
