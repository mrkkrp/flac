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

module Codec.Audio.FLAC.StreamEncoder
  ( EncoderSettings (..) )
where

import Data.Word
import Data.Default.Class

-- | Parameters of stream encoder and input to encode.

data EncoderSettings = EncoderSettings
  { encoderChannels      :: !Word32 -- ^ Number of channels, default is 2
  , encoderBitsPerSample :: !Word32 -- ^ Sample resolution in bits, default is 16
  , encoderSampleRate    :: !Word32 -- ^ Sample rate in Hz, default is 44100
  , encoderCompression   :: !Word32 -- ^ Compression level [0..8], default is 5
  , encoderBlockSize     :: !Word32 -- ^ Block size, default is 0
  , encoderDoMidSideStereo :: !Bool
    -- ^ Enable mid-side encoding on stereo input. The number of channels
    -- must be 2 for this to have any effect. Default value: 'False'.
  , encoderVerify        :: !Bool   -- ^ Verify result (slower), default is 'False'
  }

instance Default EncoderSettings where
  def = EncoderSettings
    { encoderChannels      = 2
    , encoderBitsPerSample = 16
    , encoderSampleRate    = 44100
    , encoderCompression   = 5
    , encoderBlockSize     = 0
    , encoderDoMidSideStereo = False
    , encoderVerify        = False
    }

-- https://xiph.org/flac/api/group__flac__stream__encoder.html
-- Work with files, encode directly from a file.
