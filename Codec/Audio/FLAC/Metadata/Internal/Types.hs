-- |
-- Module      :  Codec.Audio.FLAC.Metadata.Internal.Types
-- Copyright   :  © 2016–2018 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Mostly non-public metadata-specific helper types.

module Codec.Audio.FLAC.Metadata.Internal.Types
  ( MetaChain (..)
  , MetaIterator (..)
  , Metadata (..)
  , MetadataType (..)
  , MetaChainStatus (..)
  , MetaException (..)
  , ApplicationId
  , mkApplicationId
  , unApplicationId
  , SeekPoint (..)
  , CueSheetData (..)
  , CueTrack (..)
  , PictureType (..)
  , PictureData (..) )
where

import Control.Exception (Exception)
import Data.ByteString (ByteString)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Monoid ((<>))
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Void
import Foreign
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as B8

-- | An opaque newtype wrapper around @'Ptr' 'Void'@, serves to represent a
-- pointer to a metadata chain.

newtype MetaChain = MetaChain (Ptr Void)

-- | The same as 'MetaChain', this one is for pointers to metadata iterator.

newtype MetaIterator = MetaIterator (Ptr Void)

-- | The same as 'MetaChain', this one for pointers to metadata structure
-- itself.

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

-- | The exception that is thrown when manipulation of FLAC metadata fails
-- for some reason.

data MetaException
  = MetaGeneralProblem MetaChainStatus
    -- ^ General failure, see the attached 'MetaChainStatus'.
  | MetaInvalidSeekTable
    -- ^ Invalid seek table was passed to be written.
  | MetaInvalidCueSheet Text
    -- ^ Invalid CUE sheet was passed to be written. The reason why the data
    -- was invalid is attached.
  | MetaInvalidPicture Text
    -- ^ Invalid picture data was passed to be written. The reason why the
    -- data was invalid is attached.
  deriving (Eq, Show, Read)

instance Exception MetaException

-- | A normalizing wrapper around 'ByteString' that makes sure that the
-- 'ByteString' inside is a valid FLAC application name. You can create
-- values of this type using Haskell string literals with
-- @OverloadedStrings@ or with the 'mkApplicationId' smart constructor.
-- Extract the inner 'ByteString' with 'unApplicationId'.

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

-- | Single point in a seek table metadata block.

data SeekPoint = SeekPoint
  { seekPointSampleNumber :: !Word64
    -- ^ The sample number of the target frame
  , seekPointStreamOffset :: !Word64
    -- ^ The offset in bytes of the target frame with respect to beginning
    -- of the first frame
  , seekPointFrameSamples :: !Word32
    -- ^ The number of samples in the target frame
  } deriving (Eq, Ord, Show, Read)

-- | CUE sheet as stored in FLAC metadata. This differs from traditional CUE
-- sheets stored in “.cue” files (see
-- <https://hackage.haskell.org/package/cue-sheet> to work with those).

data CueSheetData = CueSheetData
  { cueCatalog :: !ByteString
    -- ^ Media catalog number, in ASCII printable characters 0x20-0x7e. In
    -- general, the media catalog number may be 0 to 128 bytes long; any
    -- unused characters will be right-padded with NUL characters. For
    -- CD-DA, this is a thirteen digit number.
  , cueLeadIn :: !Word64
    -- ^ The number of lead-in samples. This field has meaning only for
    -- CD-DA CUE sheets; for other uses it should be 0. For CD-DA, the
    -- lead-in is the TRACK 00 area where the table of contents is stored;
    -- more precisely, it is the number of samples from the first sample of
    -- the media to the first sample of the first index point of the first
    -- track. According to the Red Book, the lead-in must be silence and CD
    -- grabbing software does not usually store it; additionally, the
    -- lead-in must be at least two seconds but may be longer. For these
    -- reasons the lead-in length is stored here so that the absolute
    -- position of the first track can be computed. Note that the lead-in
    -- stored here is the number of samples up to the first index point of
    -- the first track, not necessarily to INDEX 01 of the first track; even
    -- the first track may have INDEX 00 data.
  , cueIsCd :: !Bool
    -- ^ 'True' if CUE sheet corresponds to a Compact Disc, else 'False'.
  , cueTracks :: ![CueTrack]
    -- ^ Collection of actual tracks in the CUE sheet, see 'CueTrack'.
  , cueLeadOutTrack :: !CueTrack
    -- ^ The obligatory lead-out track, will be written with the index 170.
  } deriving (Eq, Ord, Show, Read)

-- | Single track in a CUE sheet.

data CueTrack = CueTrack
  { cueTrackOffset :: !Word64
    -- ^ Track offset in samples, relative to the beginning of the FLAC
    -- audio stream. It is the offset to the first index point of the track.
    -- (Note how this differs from CD-DA, where the track's offset in the
    -- TOC is that of the track's INDEX 01 even if there is an INDEX 00.)
    -- For CD-DA, the offset must be evenly divisible by 588 samples (588
    -- samples = 44100 samples\/sec * 1\/75th of a sec).
  , cueTrackIsrc :: !ByteString
    -- ^ Track ISRC, empty if not present. This is a 12-digit alphanumeric
    -- code, the @cue-sheet@ package has corresponding type with smart
    -- constructor and validation, but for now we don't want to depend on
    -- that package.
  , cueTrackAudio :: !Bool
    -- ^ 'True' for audio tracks, 'False' for non-audio tracks.
  , cueTrackPreEmphasis :: !Bool
    -- ^ 'False' for no pre-emphasis, 'True' for pre-emphasis.
  , cueTrackPregapIndex :: !(Maybe Word64)
    -- ^ INDEX 00 (pregap) offset, see 'cueTrackIndices' for more info about
    -- indices.
  , cueTrackIndices :: !(NonEmpty Word64)
    -- ^ Track's index points. Offset in samples, relative to the track
    -- offset, of the index point. For CD-DA, the offset must be evenly
    -- divisible by 588 samples (588 samples = 44100 samples\/sec * 1\/75th
    -- of a sec). Note that the offset is from the beginning of the track,
    -- not the beginning of the audio data.
  } deriving (Eq, Ord, Show, Read)

-- | Type of picture FLAC metadata can contain. There may be several
-- metadata blocks containing pictures of different types.

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
  { pictureMimeType    :: !Text
    -- ^ The picture's MIME data. For best compatibility with players, use
    -- picture data of MIME type @image\/jpeg@ or @image\/png@.
  , pictureDescription :: !Text   -- ^ Picture's description.
  , pictureWidth       :: !Word32 -- ^ Picture's width in pixels.
  , pictureHeight      :: !Word32 -- ^ Picture's height in pixels.
  , pictureDepth       :: !Word32 -- ^ Picture's color depth in bits-per-pixel.
  , pictureColors      :: !Word32
    -- ^ For indexed palettes (like GIF), picture's number of colors (the
    -- number of palette entries), or 0 for non-indexed (i.e. 2 ^ depth).
  , pictureData        :: !ByteString -- ^ Binary picture data.
  } deriving (Eq, Ord, Show, Read)
