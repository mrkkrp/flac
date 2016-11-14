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
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module Codec.Audio.FLAC.Metadata
  ( -- * Types
    FlacMeta
  , FlacMetaSettings (..)
    -- * Manipulate FLAC metadata
  , flacMeta
  , get
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
  )
where

import Codec.Audio.FLAC.Metadata.Internal
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
import qualified Data.ByteString.Char8 as B8
import qualified Data.HashMap.Strict   as HM
import qualified Data.Text             as T

#if MIN_VERSION_base(4,9,0)
import Data.Kind (Constraint)
#else
import GHC.Exts (Constraint)
#endif

type Inner a = ExceptT MetaChainStatus (ReaderT Context IO) a

newtype FlacMeta a = FlacMeta { unFlacMeta :: Inner a }
  deriving (Functor, Applicative, Monad, MonadIO)

data Context = Context
  { metaChain    :: MetaChain
  , metaIterator :: MetaIterator
  , metaModified :: IORef Bool
  , metaVorbis   :: IORef (Either Bool Vorbis) }

data FlacMetaSettings = FlacMetaSettings
  { flacMetaSortPadding   :: Bool
  , flacMetaUsePadding    :: Bool
  , flacMetaPreserveStats :: Bool
  } deriving (Show, Read, Eq, Ord)

instance Default FlacMetaSettings where
  def = FlacMetaSettings
    { flacMetaSortPadding   = True
    , flacMetaUsePadding    = True
    , flacMetaPreserveStats = True }

class MetaValue a where -- class should not be public as stuff necessary to
  -- implement its methods won't be available to end user
  type MetaType a :: *
  -- type MetaReadable a :: Constraint -- assuming they are all readable
  -- what if we could have associated types of kind constraint that would
  -- determine what actions can be performed?
  get :: a -> FlacMeta (MetaType a)
  get = const nonsense

-- add :: MetaValue a => a -> MetaValueType a -> FlacMeta ()
-- add = undefined

-- set :: MetaValue a => a -> MetaValueType a -> FlacMeta ()
-- set = undefined

-- update :: MetaValue a => a -> (MetaValueType a -> MetaValueType a) -> FlacMeta (MetaValueType a)
-- update = undefined

-- delete :: MetaValue a => a -> FlacMeta ()
-- delete = undefined

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

----------------------------------------------------------------------------
-- Meta values

data MinBlockSize = MinBlockSize

instance MetaValue MinBlockSize where
  type MetaType MinBlockSize = Word32
  get MinBlockSize = (FlacMeta . fmap fromJust)
    (viewNum StreamInfoType (offset 4 cuint) cuint)

data MaxBlockSize = MaxBlockSize

instance MetaValue MaxBlockSize where
  type MetaType MaxBlockSize = Word32
  get MaxBlockSize = (FlacMeta . fmap fromJust)
    (viewNum StreamInfoType (offset 5 cuint) cuint)

data MinFrameSize = MinFrameSize

instance MetaValue MinFrameSize where
  type MetaType MinFrameSize = Word32
  get MinFrameSize = (FlacMeta . fmap fromJust)
    (viewNum StreamInfoType (offset 6 cuint) cuint)

data MaxFrameSize = MaxFrameSize

instance MetaValue MaxFrameSize where
  type MetaType MaxFrameSize = Word32
  get MaxFrameSize = (FlacMeta . fmap fromJust)
    (viewNum StreamInfoType (offset 7 cuint) cuint)

data SampleRate = SampleRate

instance MetaValue SampleRate where
  type MetaType SampleRate = Word32
  get SampleRate = (FlacMeta . fmap fromJust)
    (viewNum StreamInfoType (offset 8 cuint) cuint)

data Channels = Channels

instance MetaValue Channels where
  type MetaType Channels = Word32
  get Channels = (FlacMeta . fmap fromJust)
    (viewNum StreamInfoType (offset 9 cuint) cuint)

data BitsPerSample = BitsPerSample

instance MetaValue BitsPerSample where
  type MetaType BitsPerSample = Word32
  get BitsPerSample = (FlacMeta . fmap fromJust)
    (viewNum StreamInfoType (offset 10 cuint) cuint)

data TotalSamples = TotalSamples

instance MetaValue TotalSamples where
  type MetaType TotalSamples = Word64
  get TotalSamples = (FlacMeta . fmap fromJust)
    (viewNum StreamInfoType (offset 12 cuint) culong)

data MD5Sum = MD5Sum

instance MetaValue MD5Sum where
  type MetaType MD5Sum = ByteString
  get MD5Sum = (FlacMeta . fmap fromJust)
    (viewArray StreamInfoType (offset 12 cuint + offset 1 culong) 16)

data Duration = Duration

instance MetaValue Duration where
  type MetaType Duration = Double
  get Duration = do
    totalSamples <- fromIntegral <$> get TotalSamples
    sampleRate   <- fromIntegral <$> get SampleRate
    return (totalSamples / sampleRate)

data VorbisVendor = VorbisVendor

instance MetaValue VorbisVendor where
  type MetaType VorbisVendor = Maybe Text
  get VorbisVendor = FlacMeta $ do
    ensureVorbisCache
    vorbisCacheRef <- asks metaVorbis
    res <- liftIO (readIORef vorbisCacheRef)
    case res of
      Left  _ -> return Nothing
      Right x -> (return . return . vorbisVendor) x

data VorbisComment = VorbisComment VorbisField

instance MetaValue VorbisComment where
  type MetaType VorbisComment = Maybe Text
  get (VorbisComment field) = FlacMeta $ do
    ensureVorbisCache
    vorbisCacheRef <- asks metaVorbis
    res <- liftIO (readIORef vorbisCacheRef)
    case res of
      Left  _ -> return Nothing
      Right x -> (return . HM.lookup (vorbisFieldName field) . vorbisComments) x

----------------------------------------------------------------------------
-- Helpers

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

withMetaBlock :: MetadataType -> Inner a -> Inner (Maybe a)
withMetaBlock metaBlock m = do
  res <- findMetaBlock metaBlock
  if res
    then pure <$> m
    else return Nothing

-- | A helper to view simple numeric values in metadata blocks. Returns
-- 'Nothing' if it can't find requested data.

viewNum
  :: (Storable a, Integral a, Num b)
  => MetadataType      -- ^ Type of meta block to locate
  -> Int               -- ^ Offset in bytes
  -> Proxy a           -- ^ Which type to peek
  -> Inner (Maybe b)   -- ^ The result
viewNum metaBlock offset' hint = withMetaBlock metaBlock $ do
  iterator <- asks metaIterator
  fromIntegral <$> liftIO (view iterator offset' hint)

viewArray
  :: MetadataType      -- ^ Type of meta block to locate
  -> Int               -- ^ Offset in bytes
  -> Int               -- ^ Size in bytes
  -> Inner (Maybe ByteString) -- ^ The result
viewArray metaBlock offset' size = withMetaBlock metaBlock $ do
  iterator <- asks metaIterator
  liftIO (viewBA iterator offset' size)

ensureVorbisCache :: Inner ()
ensureVorbisCache = do
  vorbisCacheRef <- asks metaVorbis
  vorbisCache <- liftIO (readIORef vorbisCacheRef)
  when (vorbisCache == Left False) $ do
    x <- withMetaBlock VorbisCommentType $ do
      iterator <- asks metaIterator
      liftIO (viewVorbis iterator)
    liftIO (writeIORef vorbisCacheRef (maybe (Left True) Right x))

-- | Specify that the metadata chain has been modified.

setModified :: Inner ()
setModified = do
  modified <- asks metaModified
  liftIO (writeIORef modified True)

-- | Nonsense.

nonsense :: FlacMeta a
nonsense = FlacMeta (throwError MetaChainStatusOK)

----------------------------------------------------------------------------
-- Type hints and stuff

offset :: forall a. Storable a => Int -> Proxy a -> Int
offset n Proxy = n * sizeOf (undefined :: a)

cuint :: Proxy CUInt
cuint = Proxy

culong :: Proxy CULong
culong = Proxy

int :: Proxy Int
int = Proxy
