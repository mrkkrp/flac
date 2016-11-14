-- |
-- Module      :  Codec.Audio.FLAC.Metadata
-- Copyright   :  © 2016 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Operations on FLAC metadata.

{-# LANGUAGE CPP                        #-}
{-# LANGUAGE ConstrainedClassMethods    #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Codec.Audio.FLAC.Metadata
  ( -- * Metadata manipulation API
    FlacMeta
  , flacMeta
  , get
  , set
    -- * Meta values
  , MinBlockSize (..)
  , MaxBlockSize (..)
  , MinFrameSize (..)
  , MaxFrameSize (..)
  , SampleRate (..)
  , Channels (..)
  , BitsPerSample (..)
  , TotalSamples (..)
  , MD5Sum (..)
  , Duration (..)
  , VorbisVendor (..)
  , VorbisComment (..)
  , VorbisField (..)
  )
where

import Codec.Audio.FLAC.Metadata.Internal.Level2Interface
import Codec.Audio.FLAC.Metadata.Internal.Level2Interface.Helpers
import Codec.Audio.FLAC.Metadata.Internal.Object
import Codec.Audio.FLAC.Metadata.Internal.Types
import Codec.Audio.FLAC.Types
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Reader
import Crypto.Hash hiding (Context)
import Data.Bits
import Data.Bool (bool)
import Data.ByteString (ByteString)
import Data.Char (toUpper)
import Data.Default.Class
import Data.IORef
import Data.Maybe (fromJust)
import Data.Proxy
import Data.Text (Text)
import Foreign hiding (void)
import Foreign.C.Types
import GHC.TypeLits
import qualified Data.ByteString.Char8 as B8
import qualified Data.HashMap.Strict   as HM
import qualified Data.Text             as T

#if MIN_VERSION_base(4,9,0)
import Data.Kind (Constraint)
#else
import GHC.Exts (Constraint)
#endif

----------------------------------------------------------------------------
-- Metadata manipulation API

newtype FlacMeta a = FlacMeta { unFlacMeta :: Inner a }
  deriving (Functor, Applicative, Monad, MonadIO)

type Inner a = ExceptT MetaChainStatus (ReaderT Context IO) a

data Context = Context
  { metaChain    :: MetaChain
  , metaIterator :: MetaIterator
  , metaModified :: IORef Bool }

class MetaValue a where -- class should not be public as stuff necessary to
  -- implement its methods won't be available to end user
  type MetaType a :: *
  type MetaWriteable a :: Constraint -- assuming they are all readable
  -- what if we could have associated types of kind constraint that would
  -- determine what actions can be performed?
  get :: a -> FlacMeta (MetaType a)

  set :: MetaWriteable a => a -> MetaType a -> FlacMeta ()
  set _ _ = nonsense

-- | Manipulate FLAC metadata.

flacMeta :: MonadIO m
  => FlacMetaSettings  -- ^ Settings to use
  -> FilePath          -- ^ File to operate on
  -> FlacMeta a        -- ^ Actions to perform
  -> m (Either MetaChainStatus a) -- ^ The result
flacMeta FlacMetaSettings {..} path m = liftIO (bracket acquire release action)
  where
    acquire = (,) <$> chainNew <*> iteratorNew
    release (mchain, miterator) = do
      forM_ mchain    chainDelete
      forM_ miterator iteratorDelete
    action stuff =
      case stuff of
        (Just metaChain, Just metaIterator) -> do
          metaModified <- newIORef False
          metaVorbis   <- newIORef (Left False)
          flip runReaderT Context {..} . runExceptT $ do
            liftBool (chainRead metaChain path)
            when flacMetaSortPadding $
              liftIO (chainSortPadding metaChain)
            liftIO (iteratorInit metaIterator metaChain)
            result <- unFlacMeta m
            modified <- liftIO (readIORef metaModified)
            when modified $
              liftBool (chainWrite metaChain flacMetaUsePadding flacMetaPreserveStats)
            return result
        _ -> return (Left MetaChainStatusMemoryAllocationError)

-- wipe :: FlacMeta () -- deletes all meta data
-- wipe = undefined

helper :: IO ()
helper = do
  let path = "/home/mark/store/music/Adam Lambert/2015, The Original High/01 Ghost Town.flac"
  void . flacMeta def path $ do
    get MinBlockSize >>= liftIO . print
    get MaxBlockSize >>= liftIO . print
    get MinFrameSize >>= liftIO . print
    get MaxFrameSize >>= liftIO . print
    get SampleRate >>= liftIO . print
    get Channels >>= liftIO . print
    get BitsPerSample >>= liftIO . print
    get TotalSamples >>= liftIO . print
    digest <- digestFromByteString <$> get MD5Sum
    liftIO . print $ (digest :: Maybe (Digest MD5))
    get Duration >>= liftIO . print
    liftIO $ putStrLn "-----------------"
    get VorbisVendor >>= liftIO . print
    get (VorbisComment Artist) >>= liftIO . print
    get (VorbisComment Title) >>= liftIO . print
    get (VorbisComment Version) >>= liftIO . print
    get (VorbisComment TrackNumber) >>= liftIO . print
    get (VorbisComment TrackTotal) >>= liftIO . print
    get (VorbisComment Album) >>= liftIO . print

----------------------------------------------------------------------------
-- Meta values

data MinBlockSize = MinBlockSize

instance MetaValue MinBlockSize where
  type MetaType MinBlockSize = Word32
  type MetaWriteable MinBlockSize =
    TypeError ('Text "This attribute is not writeable.")
  get MinBlockSize = inStreamInfo getMinBlockSize

data MaxBlockSize = MaxBlockSize

instance MetaValue MaxBlockSize where
  type MetaType MaxBlockSize = Word32
  type MetaWriteable MaxBlockSize =
    TypeError ('Text "This attribute is not writeable.")
  get MaxBlockSize = inStreamInfo getMaxBlockSize

data MinFrameSize = MinFrameSize

instance MetaValue MinFrameSize where
  type MetaType MinFrameSize = Word32
  type MetaWriteable MinFrameSize =
    TypeError ('Text "This attribute is not writeable.")
  get MinFrameSize = inStreamInfo getMinFrameSize

data MaxFrameSize = MaxFrameSize

instance MetaValue MaxFrameSize where
  type MetaType MaxFrameSize = Word32
  type MetaWriteable MaxFrameSize =
    TypeError ('Text "This attribute is not writeable.")
  get MaxFrameSize = inStreamInfo getMaxFrameSize

data SampleRate = SampleRate

instance MetaValue SampleRate where
  type MetaType SampleRate = Word32
  type MetaWriteable SampleRate =
    TypeError ('Text "This attribute is not writeable.")
  get SampleRate = inStreamInfo getSampleRate

data Channels = Channels

instance MetaValue Channels where
  type MetaType Channels = Word32
  type MetaWriteable Channels =
    TypeError ('Text "This attribute is not writeable.")
  get Channels = inStreamInfo getChannels

data BitsPerSample = BitsPerSample

instance MetaValue BitsPerSample where
  type MetaType BitsPerSample = Word32
  type MetaWriteable BitsPerSample =
    TypeError ('Text "This attribute is not writeable.")
  get BitsPerSample = inStreamInfo getBitsPerSample

data TotalSamples = TotalSamples

instance MetaValue TotalSamples where
  type MetaType TotalSamples = Word64
  type MetaWriteable TotalSamples =
    TypeError ('Text "This attribute is not writeable.")
  get TotalSamples = inStreamInfo getTotalSamples

data MD5Sum = MD5Sum

instance MetaValue MD5Sum where
  type MetaType MD5Sum = ByteString
  type MetaWriteable MD5Sum =
    TypeError ('Text "This attribute is not writeable.")
  get MD5Sum = inStreamInfo getMd5Sum

data Duration = Duration

instance MetaValue Duration where
  type MetaType Duration = Double
  type MetaWriteable Duration =
    TypeError ('Text "This attribute is not writeable.")
  get Duration = do
    totalSamples <- fromIntegral <$> get TotalSamples
    sampleRate   <- fromIntegral <$> get SampleRate
    return (totalSamples / sampleRate)

data VorbisVendor = VorbisVendor

instance MetaValue VorbisVendor where
  type MetaType VorbisVendor = Maybe Text
  type MetaWriteable VorbisVendor = ()
  get VorbisVendor = FlacMeta . withMetaBlock VorbisCommentBlock $
    liftIO . (iteratorGetBlock >=> getVorbisVendor)

data VorbisComment = VorbisComment VorbisField

data VorbisField
  = Title
  | Version
  | Album
  | TrackNumber
  | TrackTotal
  | Artist
  | Performer
  | Copyright
  | License
  | Organization
  | Description
  | Genre
  | Date
  | Location
  | Contact
  | ISRC
  deriving (Show, Read, Eq, Ord, Bounded, Enum)

vorbisFieldName :: VorbisField -> Text
vorbisFieldName = T.pack . fmap toUpper . show

instance MetaValue VorbisComment where
  type MetaType VorbisComment = Maybe Text
  type MetaWriteable VorbisComment = ()
  get (VorbisComment field) = FlacMeta . fmap join . withMetaBlock VorbisCommentBlock $
    liftIO . (iteratorGetBlock >=> getVorbisComment (vorbisFieldName field))

----------------------------------------------------------------------------
-- Helpers

inStreamInfo :: (Metadata -> IO a) -> FlacMeta a
inStreamInfo f = (FlacMeta . fmap fromJust . withMetaBlock StreamInfoBlock) $
  liftIO . (iteratorGetBlock >=> f)

withMetaBlock :: MetadataType -> (MetaIterator -> Inner a) -> Inner (Maybe a)
withMetaBlock metaBlock m = do
  res      <- findMetaBlock metaBlock
  iterator <- asks metaIterator
  if res
    then pure <$> m iterator
    else return Nothing

-- | Position 'MetaIterator' on first metadata block that is of given
-- 'MetadataType'. Return 'False' if no such block found.

findMetaBlock :: MetadataType -> Inner Bool
findMetaBlock given = do
  chain    <- asks metaChain
  iterator <- asks metaIterator
  actual   <- liftIO (iteratorGetBlockType iterator)
  let go hasMore = do
        actual' <- iteratorGetBlockType iterator
        if actual' == given
          then return True
          else if hasMore
                 then iteratorNext iterator >>= go
                 else return False
  -- first try current block
  if actual == given
    then return True
    else liftIO $ do
      iteratorInit iterator chain
      go True

-- | Lift an action that may return 'Nothing' in case of failure into
-- 'FlacMeta' monad taking care of error reporting.

liftMaybe :: IO (Maybe a) -> Inner a
liftMaybe m = liftIO m >>= maybe throwStatus return

-- | Lift an action that returns 'False' on failure into 'FlacMeta' monad
-- taking care of error reporting.

liftBool :: IO Bool -> Inner ()
liftBool m = liftIO m >>= bool throwStatus (return ())

-- | Get 'MetaChainStatus' and throw it immediately.

throwStatus :: Inner a
throwStatus = do
  chain  <- asks metaChain
  status <- liftIO (chainStatus chain)
  throwError status

-- | Specify that the metadata chain has been modified.

setModified :: Inner ()
setModified = do
  modified <- asks metaModified
  liftIO (writeIORef modified True)

-- | Nonsense.

nonsense :: FlacMeta a
nonsense = FlacMeta (throwError MetaChainStatusOK)
