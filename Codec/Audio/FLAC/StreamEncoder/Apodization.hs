-- |
-- Module      :  Codec.Audio.FLAC.StreamEncoder.Apodization
-- Copyright   :  © 2016–present Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- This module exports the 'ApodizationFunction' data type, which is rarely
-- needed, and thus should not “contaminate” the
-- "Codec.Audio.FLAC.StreamEncoder" module with potentially confilcting
-- names.

module Codec.Audio.FLAC.StreamEncoder.Apodization
  ( ApodizationFunction (..) )
where

import Codec.Audio.FLAC.StreamEncoder.Internal.Types
