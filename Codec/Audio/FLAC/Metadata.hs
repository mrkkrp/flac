-- |
-- Module      :  Codec.Audio.FLAC.Metadata
-- Copyright   :  © 2016 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- The module provides a complete high-level Haskell API to manipulate FLAC
-- metadata.
--
-- === How to use this module
--
-- Just like other modules of this library, the API is file-centered — no
-- streaming support is available at this time (in libFLAC as well).
-- Retrieving and editing metadata information is very easy, you only need
-- three functions: 'runFlacMeta', 'retrieve', and @('=->')@.
--
-- Here is how to get sample rate and artist name and print them:
--
-- > import Codec.Audio.FLAC.Metadata
-- > import Control.Monad.IO.Class (MonadIO (..))
-- > import Data.Default.Class
-- >
-- > main :: IO ()
-- > main = runFlacMeta def "/path/to/my/file.flac" $ do
-- >   retrieve SampleRate             >>= liftIO . print
-- >   retrieve (VorbisComment Artist) >>= liftIO . print
--
-- Normally you would just return them packed in a tuple from the monad, of
-- course. We print the values just for demonstration.
--
-- The next example shows how to set a couple of tags:
--
-- > import Codec.Audio.FLAC.Metadata
-- > import Data.Default.Class
-- >
-- > main :: IO ()
-- > main = runFlacMeta def "/path/to/my/file.flac" $ do
-- >   VorbisComment Artist =-> Just "Alexander Scriabin"
-- >   VorbisComment Title  =-> Just "Sonata №9 “Black Mass”, Op. 68"
-- >   VorbisComment Date   =-> Nothing
--
-- Here we write two tags using the @('=->')@ operator and delete the
-- @'VorbisComment' 'Date'@ metadata attribute by setting it to 'Nothing'.
-- Note that not all attributes are writable, so we cannot set things like
-- 'SampleRate'. In fact, the type system mechanics used in the library
-- prevent this.
--
-- === Low-level details
--
-- The implementation uses the reference implementation of FLAC — libFLAC (C
-- library) under the hood. This means you'll need at least version 1.3.0 of
-- libFLAC (released 26 May 2013) installed for the binding to work.
--
-- This module in particular uses level 2 metadata interface and it's not
-- possible to choose other interface (such as level 0 and 1). This should
-- not be of any concern to end-user, however, as level 2 supports more
-- functionality than the other levels.

{-# LANGUAGE CPP                        #-}
{-# LANGUAGE ConstrainedClassMethods    #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

#if MIN_VERSION_base(4,9,0)
{-# LANGUAGE UndecidableInstances       #-}
#else
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
#endif

module Codec.Audio.FLAC.Metadata
  ( -- * Metadata manipulation API
    FlacMeta
  , FlacMetaSettings (..)
  , FlacMetaException (..)
  , MetaChainStatus (..)
  , runFlacMeta
    -- * Meta values
  , MetaValue (..)
  , MinBlockSize (..)
  , MaxBlockSize (..)
  , MinFrameSize (..)
  , MaxFrameSize (..)
  , SampleRate (..)
  , Channels (..)
  , BitsPerSample (..)
  , TotalSamples (..)
  , FileSize (..)
  , BitRate (..)
  , MD5Sum (..)
  , Duration (..)
  , Application (..)
  , SeekTable (..)
  , SeekPoint (..)
  , VorbisVendor (..)
  , VorbisComment (..)
  , VorbisField (..)
    -- * Extra functionality
  , wipeVorbisComment
  , wipeApplications
    -- * Debugging and testing
  , MetadataType (..)
  , getMetaChain )
where

import Codec.Audio.FLAC.Metadata.Internal.Level2Interface
import Codec.Audio.FLAC.Metadata.Internal.Level2Interface.Helpers
import Codec.Audio.FLAC.Metadata.Internal.Object
import Codec.Audio.FLAC.Metadata.Internal.Types
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Reader
import Data.Bool (bool)
import Data.ByteString (ByteString)
import Data.Char (toUpper)
import Data.Default.Class
import Data.IORef
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromJust, listToMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Vector (Vector)
import Foreign hiding (void)
import Prelude hiding (iterate)
import System.IO
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.List.NonEmpty    as NE

#if MIN_VERSION_base(4,9,0)
import Data.Kind (Constraint)
import GHC.TypeLits
#else
import GHC.Exts (Constraint)
#endif

----------------------------------------------------------------------------
-- Metadata manipulation API

-- | An opaque monad for reading and writing of FLAC metadata. The monad is
-- the home for 'retrieve' and @('=->')@ functions and can be run with
-- 'runFlacMeta'.

newtype FlacMeta a = FlacMeta { unFlacMeta :: Inner a }
  deriving (Functor, Applicative, Monad, MonadIO)

-- | Non-public shortcut for inner monad stack of 'FlacMeta'.

type Inner a = ReaderT Context IO a

-- | Context that 'Inner' passes around.

data Context = Context
  { metaChain    :: MetaChain     -- ^ Metadata chain
  , metaModified :: IORef Bool    -- ^ “Modified” flag
  , metaFileSize :: Integer       -- ^ Size of target file
  }

-- | Settings that control how metadata is written in FLAC file.

data FlacMetaSettings = FlacMetaSettings
  { flacMetaAutoVacuum :: !Bool
    -- ^ Whether to traverse all metadata blocks just before padding sorting
    -- (if enabled, see 'flacMetaSortPadding') and writing data to file,
    -- deleting all metadata blocks that appear to be empty, e.g. vorbis
    -- comment block without any comments (tags) in it. Default value:
    -- 'True'.
  , flacMetaSortPadding :: !Bool
    -- ^ Whether to attempt to sort and consolidate all padding at the end
    -- of metadata section. The main purpose of this is that the padding can
    -- be truncated if necessary to get more space so we can overwrite
    -- metadata blocks in place instead of overwriting the entire FLAC file.
    -- Default value: 'True'.
  , flacMetaUsePadding :: !Bool
    -- ^ This setting enables truncation of last padding metadata block if
    -- it allows to overwrite metadata in place instead of overwriting the
    -- entire file. Default value: 'True'.
  , flacMetaPreserveFileStats :: !Bool
    -- ^ If 'True', the owner and modification time will be preserved even
    -- if new FLAC file is written (this is for the cases when we need to
    -- write entire FLAC file and thus a copy of the file is written).
    -- Default value: 'True'.
  } deriving (Show, Read, Eq, Ord)

instance Default FlacMetaSettings where
  def = FlacMetaSettings
    { flacMetaAutoVacuum        = True
    , flacMetaSortPadding       = True
    , flacMetaUsePadding        = True
    , flacMetaPreserveFileStats = True }

-- | Run an action that manipulates FLAC metadata. 'FlacMetaSettings'
-- control subtle and rather low-level details of metadata editing, just
-- pass 'def' unless you know what you are doing. 'FilePath' specifies
-- location of FLAC file to read\/edit in the file system. 'FlacMeta' is a
-- monadic action that describes what to do with metadata. Compose it using
-- 'retrieve' and @('=->')@.
--
-- The action will throw 'Data.Text.Encoding.Error.UnicodeException' if text
-- data like Vorbis Comment entries cannot be read as UTF-8-encoded value.
--
-- If a problem occurs, 'FlacMetaException' is thrown with attached
-- 'MetaChainStatus' that should help investigating what went wrong.

runFlacMeta :: MonadIO m
  => FlacMetaSettings  -- ^ Settings to use
  -> FilePath          -- ^ File to operate on
  -> FlacMeta a        -- ^ Actions to perform
  -> m a               -- ^ The result
runFlacMeta FlacMetaSettings {..} path m = liftIO . withChain $ \metaChain -> do
  metaModified <- newIORef False
  metaFileSize <- withFile path ReadMode hFileSize
  flip runReaderT Context {..} $ do
    liftBool (chainRead metaChain path)
    result   <- unFlacMeta m
    modified <- liftIO (readIORef metaModified)
    when modified $ do
      when flacMetaAutoVacuum applyVacuum
      when flacMetaSortPadding $
        liftIO (chainSortPadding metaChain)
      liftBool
        (chainWrite metaChain flacMetaUsePadding flacMetaPreserveFileStats)
    return result

----------------------------------------------------------------------------
-- Meta values

-- | A class for types that specify which metadata attributes to
-- read\/write. It's not expected that users of the library will define new
-- metadata attributes other than via combination of existing ones, which is
-- also useful. For example, 'Duration' and 'BitRate' are not read from FLAC
-- file metadata directly, but defined in terms of other attributes.

class MetaValue a where

  -- | Type of data that corresponds to this metadata value. For example
  -- 'SampleRate' is represented by 'Word32' value in this library, and so
  -- @'MetaType' 'SampleRate' = 'Word32'@.

  type MetaType a :: *

  -- | Associated type of kind 'Constraint' that controls whether particular
  -- piece of metadata is writable or not.

  type MetaWritable a :: Constraint

  -- | Given value that determines what to read, read it and return. Some
  -- metadata may be missing, in that case the function typically returns a
  -- value wrapped in 'Maybe'.

  retrieve :: a -> FlacMeta (MetaType a)

  -- | Given value that determines what to write and a value to write,
  -- add\/replace a piece of metadata information. This is how you edit
  -- metadata. To delete something, set it to 'Nothing' (well, it should be
  -- something that /can be missing/, for example you cannot delete sample
  -- rate attribute). If 'MetaWritable' is defined, this method must be
  -- defined as well.

  (=->) :: MetaWritable a => a -> MetaType a -> FlacMeta ()
  _ =-> _ = error "Codec.Audio.FLAC.Metadata.(=->) is not defined"

infix 1 =->

#if MIN_VERSION_base(4,9,0)
type NotWritable = 'Text "This attribute is not writable."
#endif

-- | Minimal block size in samples used in the stream.
--
-- __Read-only__ attribute represented as a 'Word32'.

data MinBlockSize = MinBlockSize

instance MetaValue MinBlockSize where
  type MetaType MinBlockSize = Word32
#if MIN_VERSION_base(4,9,0)
  type MetaWritable MinBlockSize = TypeError NotWritable
#endif
  retrieve MinBlockSize = inStreamInfo getMinBlockSize

-- | Maximal block size in samples used in the stream. Equality of minimum
-- block size and maximum block size implies a fixed-blocksize stream.
--
-- __Read-only__ attribute represented as a 'Word32'.

data MaxBlockSize = MaxBlockSize

instance MetaValue MaxBlockSize where
  type MetaType MaxBlockSize = Word32
#if MIN_VERSION_base(4,9,0)
  type MetaWritable MaxBlockSize = TypeError NotWritable
#endif
  retrieve MaxBlockSize = inStreamInfo getMaxBlockSize

-- | Minimal frame size in bytes used in the stream. May be 0 to imply the
-- value is not known.
--
-- __Read-only__ attribute represented as a 'Word32'.

data MinFrameSize = MinFrameSize

instance MetaValue MinFrameSize where
  type MetaType MinFrameSize = Word32
#if MIN_VERSION_base(4,9,0)
  type MetaWritable MinFrameSize = TypeError NotWritable
#endif
  retrieve MinFrameSize = inStreamInfo getMinFrameSize

-- | Maximal frame size in bytes used in the stream. May be 0 to imply the
-- value is not known.
--
-- __Read-only__ attribute represented as a 'Word32'.

data MaxFrameSize = MaxFrameSize

instance MetaValue MaxFrameSize where
  type MetaType MaxFrameSize = Word32
#if MIN_VERSION_base(4,9,0)
  type MetaWritable MaxFrameSize = TypeError NotWritable
#endif
  retrieve MaxFrameSize = inStreamInfo getMaxFrameSize

-- | Sample rate in Hz.
--
-- __Read-only__ attribute represented as a 'Word32'.

data SampleRate = SampleRate

instance MetaValue SampleRate where
  type MetaType SampleRate = Word32
#if MIN_VERSION_base(4,9,0)
  type MetaWritable SampleRate = TypeError NotWritable
#endif
  retrieve SampleRate = inStreamInfo getSampleRate

-- | Number of channels. FLAC supports from 1 to 8 channels.
--
-- __Read-only__ attribute represented as a 'Word32'.

data Channels = Channels

instance MetaValue Channels where
  type MetaType Channels = Word32
#if MIN_VERSION_base(4,9,0)
  type MetaWritable Channels = TypeError NotWritable
#endif
  retrieve Channels = inStreamInfo getChannels

-- | Bits per sample (sample depth). FLAC supports from 4 to 32 bits per
-- sample. Currently the reference encoder and decoders only support up to
-- 24 bits per sample.
--
-- __Read-only__ attribute represented as a 'Word32'.

data BitsPerSample = BitsPerSample

instance MetaValue BitsPerSample where
  type MetaType BitsPerSample = Word32
#if MIN_VERSION_base(4,9,0)
  type MetaWritable BitsPerSample = TypeError NotWritable
#endif
  retrieve BitsPerSample = inStreamInfo getBitsPerSample

-- | Total number of samples in audio stream. “Samples” means inter-channel
-- sample, i.e. one second of 44.1 KHz audio will have 44100 samples
-- regardless of the number of channels. A value of zero here means the
-- number of total samples is unknown.
--
-- __Read-only__ attribute represented as a 'Word64'.

data TotalSamples = TotalSamples

instance MetaValue TotalSamples where
  type MetaType TotalSamples = Word64
#if MIN_VERSION_base(4,9,0)
  type MetaWritable TotalSamples = TypeError NotWritable
#endif
  retrieve TotalSamples = inStreamInfo getTotalSamples

-- | File size in bytes.
--
-- __Read-only__ attribute represented as an 'Integer'.

data FileSize = FileSize

instance MetaValue FileSize where
  type MetaType FileSize = Integer
#if MIN_VERSION_base(4,9,0)
  type MetaWritable FileSize = TypeError NotWritable
#endif
  retrieve FileSize = FlacMeta (asks metaFileSize)

-- | Bit rate in kilo-bits per second (kbps).
--
-- __Read-only__ attribute represented as a 'Word32'.

data BitRate = BitRate

instance MetaValue BitRate where
  type MetaType BitRate = Word32
#if MIN_VERSION_base(4,9,0)
  type MetaWritable BitRate = TypeError NotWritable
#endif
  retrieve BitRate = do
    fileSize <- fromIntegral <$> retrieve FileSize
    duration <- retrieve Duration
    -- NOTE 8 / 1000 = 125, (* 8) to get bits, (/ 1000) to get kilos
    (return . floor) (fileSize / (duration * 125))

-- | MD5 signature of the unencoded audio data. This allows the decoder to
-- determine if an error exists in the audio data even when the error does
-- not result in an invalid bitstream.
--
-- __Read-only__ attribute represented as a 'ByteString' of length 16.

data MD5Sum = MD5Sum

instance MetaValue MD5Sum where
  type MetaType MD5Sum = ByteString
#if MIN_VERSION_base(4,9,0)
  type MetaWritable MD5Sum = TypeError NotWritable
#endif
  retrieve MD5Sum = inStreamInfo getMd5Sum

-- | Duration in seconds.
--
-- __Read-only__ attribute represented as a 'Double'.

data Duration = Duration

instance MetaValue Duration where
  type MetaType Duration = Double
#if MIN_VERSION_base(4,9,0)
  type MetaWritable Duration = TypeError NotWritable
#endif
  retrieve Duration = do
    totalSamples <- fromIntegral <$> retrieve TotalSamples
    sampleRate   <- fromIntegral <$> retrieve SampleRate
    return (totalSamples / sampleRate)

-- | Application metadata. The 'ByteString' argument to 'Application' data
-- constructor (application's ID) should be four bytes long. If it's too
-- short, null bytes will be appended to it to make it four bytes long. If
-- it's too long, it will be truncated.
--
-- For the list of defined application IDs, see:
--
-- <https://xiph.org/flac/id.html>.
--
-- __Writable__ optional attribute represented as a @'Maybe' 'ByteString'@.

data Application = Application ByteString

instance MetaValue Application where
  type MetaType Application = Maybe ByteString
  type MetaWritable Application = ()
  retrieve (Application appId) =
    FlacMeta . withApplicationBlock (fixAppId appId) $
      liftIO . (iteratorGetBlock >=> getApplicationData)
  Application appId =-> Nothing =
    void . FlacMeta . withApplicationBlock (fixAppId appId) $ \i -> do
      liftBool (iteratorDeleteBlock i)
      setModified
  Application appId =-> Just data' =
    FlacMeta . withApplicationBlock' (fixAppId appId) $ \i -> do
      block <- liftIO (iteratorGetBlock i)
      liftIO (setApplicationId block appId)
      liftBool (setApplicationData block data')
      setModified

-- | Seek table as a 'Vector' of 'SeekPoint's. Seek points within a table
-- must be sorted in ascending order by sample number. If you try to write
-- an incorrect seek table, 'FlacMetaException' will be raised with
-- 'FlacMetaIncorrectSeekTable' constructor.
--
-- __Writable__ optional attribute represented as a @'Maybe' ('Vector'
-- 'SeekPoint')@.

data SeekTable = SeekTable

instance MetaValue SeekTable where
  type MetaType SeekTable = Maybe (Vector SeekPoint)
  type MetaWritable SeekTable = ()
  retrieve SeekTable =
    FlacMeta . withMetaBlock SeekTableBlock $
      liftIO . (iteratorGetBlock >=> getSeekPoints)
  SeekTable =-> Nothing =
    void . FlacMeta . withMetaBlock SeekTableBlock $ \i -> do
      liftBool (iteratorDeleteBlock i)
      setModified
  SeekTable =-> Just seekPoints =
    FlacMeta . withMetaBlock' SeekTableBlock $ \i -> do
      block <- liftIO (iteratorGetBlock i)
      liftBool (setSeekPoints block seekPoints)
      setModified

-- | Vorbis “vendor” comment. When “Vorbis Comment” metadata block is
-- present, the “vendor” entry is always in there, so when you delete it (by
-- @'VorbisVendor' '=->' 'Nothing'@), you really set it to an empty string
-- (which is enough to trigger auto vacuum feature if no other entries are
-- detected, see 'flacMetaAutoVacuum').
--
-- __Writable__ optional attribute represented as a @'Maybe' 'Text'@.

data VorbisVendor = VorbisVendor

instance MetaValue VorbisVendor where
  type MetaType VorbisVendor = Maybe Text
  type MetaWritable VorbisVendor = ()
  retrieve VorbisVendor =
    FlacMeta . withMetaBlock VorbisCommentBlock $
      liftIO . (iteratorGetBlock >=> getVorbisVendor)
  VorbisVendor =-> Nothing =
    void . FlacMeta . withMetaBlock VorbisCommentBlock $ \i -> do
      block <- liftIO (iteratorGetBlock i)
      liftBool (setVorbisVendor block "")
      setModified
  VorbisVendor =-> (Just value) =
    FlacMeta . withMetaBlock' VorbisCommentBlock $ \i -> do
      block <- liftIO (iteratorGetBlock i)
      liftBool (setVorbisVendor block value)
      setModified

-- | Various Vorbis comments, see 'VorbisField' for available field names.
-- The field names are mostly taken from here:
--
-- <https://www.xiph.org/vorbis/doc/v-comment.html>.
--
-- 'TrackTotal', 'DiscNumber', and 'Rating' are popular de-facto standard
-- fields. The library also supports the standard ReplayGain comments.
--
-- __Writable__ optional attribute represented as a @'Maybe' 'Text'@.

data VorbisComment = VorbisComment VorbisField

-- | Enumeration of all supported filed names to index vorbis comment
-- entries.

data VorbisField
  = Title              -- ^ Track\/Work name.
  | Version            -- ^ The version field may be used to differentiate
                       -- multiple versions of the same track title in a
                       -- single collection (e.g. remix info).
  | Album              -- ^ The collection name to which this track belongs
  | TrackNumber        -- ^ The track number of this piece if part of a
                       -- specific larger collection or album.
  | TrackTotal         -- ^ Total number of tracks in the collection this
                       -- track belongs to.
  | DiscNumber         -- ^ Disc number in a multi-disc release.
  | Artist             -- ^ The artist generally considered responsible for
                       -- the work. In popular music this is usually the
                       -- performing band or singer. For classical music it
                       -- would be the composer. For an audio book it would
                       -- be the author of the original text.
  | Performer          -- ^ The artist(s) who performed the work. In
                       -- classical music this would be the conductor,
                       -- orchestra, soloists. In an audio book it would be
                       -- the actor who did the reading. In popular music
                       -- this is typically the same as the 'Artist' and is
                       -- omitted.
  | Copyright          -- ^ Copyright attribution, e.g., “2001 Nobody's
                       -- Band” or “1999 Jack Moffitt”.
  | License            -- ^ License information, e.g., “All Rights
                       -- Reserved”, “Any Use Permitted”, a URL to a license
                       -- such as a Creative Commons license or the EFF Open
                       -- Audio License, etc.
  | Organization       -- ^ Name of the organization producing the track
                       -- (i.e. the “record label”).
  | Description        -- ^ A short text description of the contents.
  | Genre              -- ^ A short text indication of music genre.
  | Date               -- ^ Date the track was recorded, usually year.
  | Location           -- ^ Location where track was recorded.
  | Contact            -- ^ Contact information for the creators or
                       -- distributors of the track. This could be a URL, an
                       -- email address, the physical address of the
                       -- producing label.
  | ISRC               -- ^ ISRC number for the track, see <http://isrc.ifpi.org/en>.
  | Rating             -- ^ Rating, usually mapped as 1–5 stars with actual
                       -- values “20”, “40”, “60”, “80”, “100” stored.
  | RGTrackPeak        -- ^ Replay gain track peak, e.g. “0.99996948”.
  | RGTrackGain        -- ^ Replay gain track gain, e.g. “-7.89 dB”.
  | RGAlbumPeak        -- ^ Replay gain album peak, e.g. “0.99996948”.
  | RGAlbumGain        -- ^ Replay gain album gain, e.g. “-7.89 dB”.
  deriving (Show, Read, Eq, Ord, Bounded, Enum)

instance MetaValue VorbisComment where
  type MetaType VorbisComment = Maybe Text
  type MetaWritable VorbisComment = ()
  retrieve (VorbisComment field) =
    FlacMeta . fmap join . withMetaBlock VorbisCommentBlock $
      liftIO . (iteratorGetBlock >=> getVorbisComment (vorbisFieldName field))
  VorbisComment field =-> Nothing =
    void . FlacMeta . withMetaBlock VorbisCommentBlock $ \i -> do
      block <- liftIO (iteratorGetBlock i)
      liftBool (deleteVorbisComment (vorbisFieldName field) block)
      setModified
  VorbisComment field =-> Just value =
    FlacMeta . withMetaBlock' VorbisCommentBlock $ \i -> do
      block <- liftIO (iteratorGetBlock i)
      liftBool (setVorbisComment (vorbisFieldName field) value block)
      setModified

----------------------------------------------------------------------------
-- Extra functionality

-- | Delete all “Vorbis comment” metadata blocks.

wipeVorbisComment :: FlacMeta ()
wipeVorbisComment =
  void . FlacMeta . withMetaBlock VorbisCommentBlock $ \i -> do
    liftBool (iteratorDeleteBlock i)
    setModified

-- | Delete all “Application” metadata blocks.

wipeApplications :: FlacMeta ()
wipeApplications =
  void . FlacMeta . withMetaBlock ApplicationBlock $ \i -> do
    liftBool (iteratorDeleteBlock i)
    setModified

----------------------------------------------------------------------------
-- Debugging and testing

-- | Return list of all 'MetadataType's of metadata blocks detected in
-- order.

getMetaChain :: FlacMeta (NonEmpty MetadataType)
getMetaChain = FlacMeta $ do
  chain <- asks metaChain
  NE.fromList <$> withIterator chain (fmap Just . liftIO . iteratorGetBlockType)

----------------------------------------------------------------------------
-- Helpers

-- | A helper that takes a function that extracts something from 'Metadata'
-- block. It finds the 'StreamInfoBlock', gets 'Metadata' from it and
-- applies given function to get the final value.

inStreamInfo :: (Metadata -> IO a) -> FlacMeta a
inStreamInfo f =
  FlacMeta . fmap fromJust . withMetaBlock StreamInfoBlock $
    liftIO . (iteratorGetBlock >=> f)

-- | Given 'MetadataType' (type of metadata block) and an action that uses
-- an iterator which points to a block of specified type, perform that
-- action and return its result wrapped in 'Just' if block of requested type
-- was found, 'Nothing' otherwise. If there are several blocks of given
-- type, action will be performed for each of them, but only first result
-- will be returned.

withMetaBlock
  :: MetadataType      -- ^ Type of block to find
  -> (MetaIterator -> Inner a) -- ^ What to do if such block found
  -> Inner (Maybe a)   -- ^ Result in 'Just' if block was found
withMetaBlock given f = do
  chain <- asks metaChain
  fmap listToMaybe . withIterator chain $ \i -> do
    actual <- liftIO (iteratorGetBlockType i)
    if actual == given
      then Just <$> f i
      else return Nothing

-- | Just like 'withMetaBlock', but creates a new block of requested type if
-- no block of such type can be found.

withMetaBlock'
  :: MetadataType      -- ^ Type of block to find
  -> (MetaIterator -> Inner a) -- ^ What to do if such block found
  -> Inner a           -- ^ Result
withMetaBlock' blockType f = do
  chain <- asks metaChain
  res   <- withMetaBlock blockType f
  case res of
    Nothing -> fmap head . withIterator chain $ \i -> do
      actual <- liftIO (iteratorGetBlockType i)
      if actual == StreamInfoBlock
        then
          let acquire = liftMaybe (objectNew blockType)
              release = liftIO . objectDelete
          in bracketOnError acquire release $ \block -> do
               liftBool (iteratorInsertBlockAfter i block)
               Just <$> f i
        else return Nothing
    Just x -> return x

-- | The same as 'withMetaBlock' but it searches for a block of type
-- 'ApplicationBlock' having specified application id.

withApplicationBlock
  :: ByteString        -- ^ Application id to find
  -> (MetaIterator -> Inner a) -- ^ What to do if such block found
  -> Inner (Maybe a)   -- ^ Result in 'Just' if block was found
withApplicationBlock givenId f = do
  chain <- asks metaChain
  fmap listToMaybe . withIterator chain $ \i -> do
    actual <- liftIO (iteratorGetBlockType i)
    if actual == ApplicationBlock
      then do
        block    <- liftIO (iteratorGetBlock i)
        actualId <- liftIO (getApplicationId block)
        if actualId == givenId
          then Just <$> f i
          else return Nothing
      else return Nothing

-- | Just like 'applicationBlock', but creates a new application block with
-- given id if no such block can be found.

withApplicationBlock'
  :: ByteString        -- ^ Application id to find
  -> (MetaIterator -> Inner a) -- ^ What to do if such block found
  -> Inner a           -- ^ Result
withApplicationBlock' givenId f = do
  chain <- asks metaChain
  res   <- withApplicationBlock givenId f
  case res of
    Nothing -> fmap head . withIterator chain $ \i -> do
      actual <- liftIO (iteratorGetBlockType i)
      if actual == StreamInfoBlock
        then
          let acquire = liftMaybe (objectNew ApplicationBlock)
              release = liftIO . objectDelete
          in bracketOnError acquire release $ \block -> do
               liftIO (setApplicationId block givenId)
               liftBool (iteratorInsertBlockAfter i block)
               Just <$> f i
        else return Nothing
    Just x -> return x

-- | Go through all metadata blocks and delete empty ones.

applyVacuum :: Inner ()
applyVacuum = do
  chain <- asks metaChain
  void . withIterator chain $ \i -> do
    blockType <- liftIO (iteratorGetBlockType i)
    block     <- liftIO (iteratorGetBlock     i)
    empty     <- liftIO (isMetaBlockEmpty blockType block)
    when empty $
      liftBool (iteratorDeleteBlock i)
    return Nothing

-- | Determine if given metadata block is empty.

isMetaBlockEmpty :: MonadIO m => MetadataType -> Metadata -> m Bool
isMetaBlockEmpty VorbisCommentBlock block =
  liftIO (isVorbisCommentEmpty block)
isMetaBlockEmpty _ _ = return False

-- | Lift an action that may return 'Nothing' on failure into 'Inner' monad
-- taking care of error reporting.

liftMaybe :: IO (Maybe a) -> Inner a
liftMaybe m = liftIO m >>= maybe throwStatus return

-- | Lift an action that returns 'False' on failure into 'Inner' monad
-- taking care of error reporting.

liftBool :: IO Bool -> Inner ()
liftBool m = liftIO m >>= bool throwStatus (return ())

-- | Get 'MetaChainStatus' and throw it immediately.

throwStatus :: Inner a
throwStatus = do
  chain  <- asks metaChain
  status <- liftIO (chainStatus chain)
  throwM (FlacMetaGeneralProblem status)

-- | Specify that the metadata chain has been modified.

setModified :: Inner ()
setModified = do
  modified <- asks metaModified
  liftIO (writeIORef modified True)

-- | Map 'VorbisField' to its ASCII name in form of a 'ByteString'.

vorbisFieldName :: VorbisField -> ByteString
vorbisFieldName RGTrackPeak = "REPLAYGAIN_TRACK_PEAK"
vorbisFieldName RGTrackGain = "REPLAYGAIN_TRACK_GAIN"
vorbisFieldName RGAlbumPeak = "REPLAYGAIN_ALBUM_PEAK"
vorbisFieldName RGAlbumGain = "REPLAYGAIN_ALBUM_GAIN"
vorbisFieldName field = (B8.pack . fmap toUpper . show) field

-- | Fix application id so it's exactly 4 bytes long.

fixAppId :: ByteString -> ByteString
fixAppId appId = B.take 4 (appId <> B.replicate 4 0x00)
