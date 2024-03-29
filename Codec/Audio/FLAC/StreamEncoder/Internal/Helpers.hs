{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Codec.Audio.FLAC.StreamEncoder.Internal.Helpers
-- Copyright   :  © 2016–present Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Wrappers around helpers written to help work with the stream encoder.
module Codec.Audio.FLAC.StreamEncoder.Internal.Helpers
  ( encoderProcessHelper,
    renderApodizationSpec,
  )
where

import Codec.Audio.FLAC.StreamEncoder.Internal.Types
import Data.ByteString (ByteString)
import Data.ByteString.Builder qualified as BU
import Data.ByteString.Lazy qualified as BL
import Data.List (intersperse)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Word (Word64)
import Foreign.C.String
import Numeric.Natural

-- | Encode given input file, return 'False' in case of failure.
encoderProcessHelper ::
  -- | 'Encoder' to use
  Encoder ->
  -- | Offset of data chunk
  Word64 ->
  -- | Size of data chunk
  Word64 ->
  -- | Location of input file (normalized)
  FilePath ->
  -- | 'False' in case of trouble
  IO Bool
encoderProcessHelper encoder dataOffset dataSize ipath =
  withCString ipath $ \ipathPtr ->
    c_encoder_process_helper
      encoder -- stream encoder
      dataOffset -- offset of data chunk
      dataSize -- size of data chunk
      ipathPtr -- path to input file

foreign import ccall unsafe "FLAC__stream_encoder_process_helper"
  c_encoder_process_helper :: Encoder -> Word64 -> Word64 -> CString -> IO Bool

-- | Render apodization functions specification as per description here:
-- <https://xiph.org/flac/api/group__flac__stream__encoder.html#ga6598f09ac782a1f2a5743ddf247c81c8>.
renderApodizationSpec :: NonEmpty ApodizationFunction -> ByteString
renderApodizationSpec =
  BL.toStrict
    . BU.toLazyByteString
    . mconcat
    . intersperse ";"
    . NE.toList
    . fmap f
  where
    f :: ApodizationFunction -> BU.Builder
    f Bartlett = "bartlett"
    f BartlettHann = "bartlett_hann"
    f Blackman = "blackman"
    f BlackmanHarris4Term92Db = "blackman_harris_4term_92db"
    f Connes = "connes"
    f Flattop = "flattop"
    f (Gauss stddev) = "gauss(" <> BU.doubleDec stddev <> ")"
    f Hamming = "hamming"
    f Hann = "hann"
    f KaiserBessel = "kaiser_bessel"
    f Nuttall = "nuttall"
    f Rectangle = "rectangle"
    f Triangle = "triangle"
    f (Tukey p) = "tukey(" <> BU.doubleDec p <> ")"
    f (PartialTukey n Nothing) =
      "partial_tukey(" <> natDec n <> ")"
    f (PartialTukey n (Just (ov, Nothing))) =
      "partial_tukey(" <> natDec n <> "/" <> BU.doubleDec ov <> ")"
    f (PartialTukey n (Just (ov, Just p))) =
      "partial_tukey(" <> natDec n <> "/" <> BU.doubleDec ov <> "/" <> BU.doubleDec p <> ")"
    f (PunchoutTukey n Nothing) =
      "punchout_tukey(" <> natDec n <> ")"
    f (PunchoutTukey n (Just (ov, Nothing))) =
      "punchout_tukey(" <> natDec n <> "/" <> BU.doubleDec ov <> ")"
    f (PunchoutTukey n (Just (ov, Just p))) =
      "punchout_tukey(" <> natDec n <> "/" <> BU.doubleDec ov <> "/" <> BU.doubleDec p <> ")"
    f Welch = "welch"
    natDec :: Natural -> BU.Builder
    natDec = BU.integerDec . fromIntegral
