-- |
-- Module      :  Codec.Audio.FLAC.Metadata.CueSheet
-- Copyright   :  © 2016 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- This module exports the 'CueSheet' data type, which is rarely needed, and
-- thus should not “contaminate” the "Codec.Audio.Metadata" module with
-- potentially conflicting names.

module Codec.Audio.FLAC.Metadata.CueSheet
  ( CueSheetData (..) )
where

import Codec.Audio.FLAC.Metadata.Internal.Types
