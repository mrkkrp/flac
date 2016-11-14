-- |
-- Module      :  Codec.Audio.FLAC.Metadata.Internal.Types
-- Copyright   :  © 2016 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Non-public metadata-specific helper types.

module Codec.Audio.FLAC.Metadata.Internal.Types
  ( MetaChain (..)
  , MetaIterator (..)
  , Metadata (..)
  , MetadataType (..) )
where

import Data.Void
import Foreign

-- | An opaque newtype wrapper around 'Ptr' 'Void', serves to represent
-- pointer to metadata chain.

newtype MetaChain = MetaChain (Ptr Void)

-- | Same as 'MetaChain', this one for pointer to metadata iterator.

newtype MetaIterator = MetaIterator (Ptr Void)

-- | Same as 'MetaChain', this one for pointer to metadata structure itself.

newtype Metadata = Metadata (Ptr Void)

-- | Enumeration of all known metadata blocks.

data MetadataType
  = StreamInfoBlock    -- ^ Stream info block (general data like sample rate)
  | PaddingBlock       -- ^ Padding block
  | ApplicationBlock   -- ^ Application block
  | SeektableBlock     -- ^ Seektable block
  | VorbisCommentBlock -- ^ Vorbis comment block, a.k.a. FLAC tags
  | CueSheetType       -- ^ Cue sheet block
  | PictureType        -- ^ Picture block
  | UndefinedType      -- ^ Undefined block
  deriving (Show, Read, Eq, Ord, Bounded, Enum)
