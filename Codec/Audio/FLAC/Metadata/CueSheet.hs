-- |
-- Module      :  Codec.Audio.FLAC.Metadata.CueSheet
-- Copyright   :  © 2016–2017 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- This module exports the 'CueSheetData' and 'CueTrack' data types, which
-- are rarely needed, and thus should not “contaminate” the
-- "Codec.Audio.Metadata" module with potentially conflicting names.

module Codec.Audio.FLAC.Metadata.CueSheet
  ( CueSheetData (..)
  , CueTrack (..) )
where

import Codec.Audio.FLAC.Metadata.Internal.Types
