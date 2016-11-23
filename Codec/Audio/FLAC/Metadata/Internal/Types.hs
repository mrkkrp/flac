-- |
-- Module      :  Codec.Audio.FLAC.Metadata.Internal.Types
-- Copyright   :  © 2016 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Mostly non-public metadata-specific helper types.

{-# LANGUAGE CPP             #-}
{-# LANGUAGE RecordWildCards #-}

module Codec.Audio.FLAC.Metadata.Internal.Types
  ( MetaChain (..)
  , MetaIterator (..)
  , Metadata (..)
  , MetadataType (..)
  , FlacMetaException (..)
  , MetaChainStatus (..)
  , ApplicationId
  , mkApplicationId
  , unApplicationId
  , SeekPoint (..)
  , PictureType (..)
  , PictureData (..) )
where

import Control.Exception (Exception)
import Data.ByteString (ByteString)
import Data.Monoid ((<>))
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Void
import Foreign
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as B8

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
  | SeekTableBlock     -- ^ Seek table block
  | VorbisCommentBlock -- ^ Vorbis comment block, a.k.a. FLAC tags
  | CueSheetBlock      -- ^ Cue sheet block
  | PictureBlock       -- ^ Picture block
  | UndefinedBlock     -- ^ Undefined block
  deriving (Show, Read, Eq, Ord, Bounded, Enum)

-- | Exception that is thrown when manipulation of FLAC metadata failed for
-- some reason.

data FlacMetaException
  = FlacMetaGeneralProblem MetaChainStatus
    -- ^ General failure, see the attached 'MetaChainStatus'
  | FlacMetaIncorrectSeekTable
    -- ^ Incorrect seek table was passed to be written
  deriving (Eq, Show, Read)

instance Exception FlacMetaException

-- | Enumeration of meta chain statuses.

data MetaChainStatus
  = MetaChainStatusOK
    -- ^ The chain is in the normal OK state.
  | MetaChainStatusIllegalInput
    -- ^ The data passed into a function violated the function's usage
    -- criteria.
  | MetaChainStatusErrorOpeningFile
    -- ^ The chain could not open the target file.
  | MetaChainStatusNotFlacFile
    -- ^ The chain could not find the FLAC signature at the start of the
    -- file.
  | MetaChainStatusNotWritable
    -- ^ The chain tried to write to a file that was not writable.
  | MetaChainStatusBadMetadata
    -- ^ The chain encountered input that does not conform to the FLAC
    -- metadata specification.
  | MetaChainStatusReadError
    -- ^ The chain encountered an error while reading the FLAC file.
  | MetaChainStatusSeekError
    -- ^ The chain encountered an error while seeking in the FLAC file.
  | MetaChainStatusWriteError
    -- ^ The chain encountered an error while writing the FLAC file.
  | MetaChainStatusRenameError
    -- ^ The chain encountered an error renaming the FLAC file.
  | MetaChainStatusUnlinkError
    -- ^ The chain encountered an error removing the temporary file.
  | MetaChainStatusMemoryAllocationError
    -- ^ Memory allocation failed.
  | MetaChainStatusInternalError
    -- ^ The caller violated an assertion or an unexpected error occurred.
  | MetaChainStatusInvalidCallbacks
    -- ^ One or more of the required callbacks was NULL.
  | MetaChainStatusReadWriteMismatch
    -- ^ This error occurs when read and write methods do not match (i.e.
    -- when if you read with callbacks, you should also use function that
    -- writes with callbacks).
  | MetaChainStatusWrongWriteCall
    -- ^ Should not ever happen when you use this binding.
  deriving (Show, Read, Eq, Ord, Bounded, Enum)

-- | A normalizing wrapper around 'ByteString' that makes sure that the
-- 'ByteString' inside is a valid FLAC application name. You can create
-- values of this type using Haskell string literals with
-- @OverloadedStrings@ or with 'mkApplicationId'. Extract the inner
-- 'ByteString' with 'unApplicationId'.

newtype ApplicationId = ApplicationId ByteString
  deriving (Eq, Ord)

instance Show ApplicationId where
  show (ApplicationId appId) = show appId

instance IsString ApplicationId where
  fromString = mkApplicationId . B8.pack

-- | Application id must be four bytes long. If it's too short, null bytes
-- will be appended to it to make it four bytes long. If it's too long, it
-- will be truncated.

mkApplicationId :: ByteString -> ApplicationId
mkApplicationId appId = ApplicationId $
  B.take 4 (appId <> B.replicate 4 0x00)

-- | Get 'ByteString' from 'ApplicationId'.

unApplicationId :: ApplicationId -> ByteString
unApplicationId (ApplicationId appId) = appId

-- | The datatype represents a single point in a seek table metadata block.

data SeekPoint = SeekPoint
  { seekPointSampleNumber :: !Word64
    -- ^ The sample number of the target frame
  , seekPointStreamOffset :: !Word64
    -- ^ The offset in bytes of the target frame with respect to beginning
    -- of the first frame
  , seekPointFrameSamples :: !Word32
    -- ^ The number of samples in the target frame
  } deriving (Eq, Ord, Show, Read)

instance Storable SeekPoint where
  -- NOTE The values are correct for my x86_64 machine, but they are here
  -- just to remove the warning, we don't use them in this binding, as it's
  -- not reliable and on machines with different architectures they may be
  -- simply incorrect. So to read an array of values, we use a helper that
  -- returns pointer to Nth element of array and recreate it this way.
  sizeOf    _ = 24
  alignment _ = 8
  peek ptr = do
    seekPointSampleNumber <- peekByteOff ptr 0
    seekPointStreamOffset <- peekByteOff ptr 8
    seekPointFrameSamples <- peekByteOff ptr 16
    return SeekPoint {..}
  poke ptr SeekPoint {..} = do
    pokeByteOff ptr  0 seekPointSampleNumber
    pokeByteOff ptr  8 seekPointStreamOffset
    pokeByteOff ptr 16 seekPointFrameSamples

-- | Type of picture FLAC metadata can contain. There may be several
-- metadata blocks containing pictures of different “types”.

data PictureType
  = PictureOther             -- ^ Other
  | PictureFileIconStandard  -- ^ 32×32 pixels file icon (PNG only)
  | PictureFileIcon          -- ^ Other file icon
  | PictureFrontCover        -- ^ Cover (front)
  | PictureBackCover         -- ^ Cover (back)
  | PictureLeafletPage       -- ^ Leaflet page
  | PictureMedia             -- ^ Media (e.g. label side of CD)
  | PictureLeadArtist        -- ^ Lead artist\/lead performer\/soloist
  | PictureArtist            -- ^ Artist\/performer
  | PictureConductor         -- ^ Conductor
  | PictureBand              -- ^ Band\/orchestra
  | PictureComposer          -- ^ Composer
  | PictureLyricist          -- ^ Lyricist\/text writer
  | PictureRecordingLocation -- ^ Recording location
  | PictureDuringRecording   -- ^ During recording
  | PictureDuringPerformance -- ^ During performance
  | PictureVideoScreenCapture -- ^ Movie\/video screen capture
  | PictureFish              -- ^ A bright coloured fish
  | PictureIllustration      -- ^ Illustration
  | PictureBandLogotype      -- ^ Band\/artist logotype
  | PicturePublisherLogotype -- ^ Publisher\/studio logotype
  deriving (Show, Read, Eq, Ord, Bounded, Enum)

-- | Representation of picture contained in a FLAC metadata block.

data PictureData = PictureData
  { pictureMimeType    :: Text
    -- ^ The picture's MIME data. For best compatibility with players, use
    -- picture data of MIME type @image\/jpeg@ or @image\/png@.
  , pictureDescription :: Text   -- ^ Picture's description.
  , pictureWidth       :: Word32 -- ^ Picture's width in pixels.
  , pictureHeight      :: Word32 -- ^ Picture's height in pixels.
  , pictureDepth       :: Word32 -- ^ Picture's color depth in bits-per-pixel.
  , pictureColors      :: Word32
    -- ^ For indexed palettes (like GIF), picture's number of colors (the
    -- number of palette entries), or 0 for non-indexed (i.e. 2 ^ depth).
  , pictureData        :: ByteString -- ^ Binary picture data.
  } deriving (Eq, Ord, Show, Read)
