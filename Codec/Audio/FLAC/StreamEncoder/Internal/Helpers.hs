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
  ( encoderProcessHelper
  , renderApodizationSpec )
where

import Codec.Audio.FLAC.StreamEncoder.Internal.Types
import Data.ByteString (ByteString)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Monoid ((<>))
import Data.Word (Word64)
import Foreign.C.String
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.List.NonEmpty    as NE

-- | Encode given input file, return 'False' in case of failure.

encoderProcessHelper
  :: Encoder           -- ^ 'Encoder' to use
  -> Word64            -- ^ Offset of data chunk
  -> Word64            -- ^ Size of data chunk
  -> FilePath          -- ^ Location of input file (normalized)
  -> IO Bool           -- ^ 'False' in case of trouble
encoderProcessHelper encoder dataOffset dataSize ipath =
  withCString ipath $ \ipathPtr ->
    c_encoder_process_helper
      encoder             -- stream encoder
      dataOffset          -- offset of data chunk
      dataSize            -- size of data chunk
      ipathPtr            -- path to input file

foreign import ccall unsafe "FLAC__stream_encoder_process_helper"
  c_encoder_process_helper :: Encoder -> Word64 -> Word64 -> CString -> IO Bool

-- | Render apodization functions specification as per description here:
-- <https://xiph.org/flac/api/group__flac__stream__encoder.html#ga6598f09ac782a1f2a5743ddf247c81c8>.

renderApodizationSpec :: NonEmpty ApodizationFunction -> ByteString
renderApodizationSpec = B.intercalate ";" . NE.toList . fmap f
  where
    f Bartlett                = "bartlett"
    f BartlettHann            = "bartlett_hann"
    f Blackman                = "blackman"
    f BlackmanHarris4Term92Db = "blackman_harris_4term_92db"
    f Connes                  = "connes"
    f Flattop                 = "flattop"
    f (Gauss stddev)          = "gauss(" <> showbs stddev <> ")"
    f Hamming                 = "hamming"
    f Hann                    = "hann"
    f KaiserBessel            = "kaiser_bessel"
    f Nuttall                 = "nuttall"
    f Rectangle               = "rectangle"
    f Triangle                = "triangle"
    f (Tukey p)               = "tukey(" <> showbs p <> ")"
    f (PartialTukey n Nothing)
      = "partial_tukey(" <> showbs n <> ")"
    f (PartialTukey n (Just (ov, Nothing)))
      = "partial_tukey(" <> showbs n <> "/" <> showbs ov <> ")"
    f (PartialTukey n (Just (ov, Just p)))
      = "partial_tukey(" <> showbs n <> "/" <> showbs ov <> "/" <> showbs p <> ")"
    f (PunchoutTukey n Nothing)
      = "punchout_tukey(" <> showbs n <> ")"
    f (PunchoutTukey n (Just (ov, Nothing)))
      = "punchout_tukey(" <> showbs n <> "/" <> showbs ov <> ")"
    f (PunchoutTukey n (Just (ov, Just p)))
      = "punchout_tukey(" <> showbs n <> "/" <> showbs ov <> "/" <> showbs p <> ")"
    f Welch                   = "welch"
    showbs :: Show a => a -> ByteString
    showbs = B8.pack . show
