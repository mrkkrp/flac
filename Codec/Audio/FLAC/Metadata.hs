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
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

module Codec.Audio.FLAC.Metadata
  ( -- * Types
    FlacMeta
  , FlacMetaSettings (..)
    -- * Manipulate FLAC metadata
  , flacMeta
  , get
    -- * Meta values
  )
where

import Codec.Audio.FLAC.Metadata.Internal
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Reader
import Data.Bool (bool)
import Data.Default.Class
import Data.IORef
import Data.Proxy
import Foreign
import Foreign.C.Types

#if MIN_VERSION_base(4,9,0)
import Data.Kind (Constraint)
#else
import GHC.Exts (Constraint)
#endif

type Inner a = ExceptT MetaChainStatus (ReaderT Context IO) a

newtype FlacMeta a = FlacMeta { unFlacMeta :: Inner a }
  deriving (Functor, Applicative, Monad)

data Context = Context
  { metaChain    :: MetaChain
  , metaIterator :: MetaIterator
  , metaModified :: IORef Bool }

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
  type MetaReadable a :: Constraint
  -- what if we could have associated types of kind constraint that would
  -- determine what actions can be performed?
  get :: MetaReadable a => a -> FlacMeta (MetaType a)
  get = undefined

-- add :: MetaValue a => a -> MetaValueType a -> FlacMeta ()
-- add = undefined

-- set :: MetaValue a => a -> MetaValueType a -> FlacMeta ()
-- set = undefined

-- update :: MetaValue a => a -> (MetaValueType a -> MetaValueType a) -> FlacMeta (MetaValueType a)
-- update = undefined

-- delete :: MetaValue a => a -> FlacMeta ()
-- delete = undefined

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
          flip runReaderT Context {..} . runExceptT $ do
            liftBool (chainRead metaChain path)
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
  result <- flacMeta def path $ do
    get SampleRate
  print result

----------------------------------------------------------------------------
-- Meta values

data SampleRate = SampleRate

instance MetaValue SampleRate where
  type MetaType     SampleRate = Int
  type MetaReadable SampleRate = ()
  get SampleRate = FlacMeta $ do
    -- TODO need a helper for this
    iterator <- asks metaIterator
    fromIntegral <$> liftIO (view iterator (8 * sizeOf (0 :: CUInt)) (Proxy :: Proxy CUInt))

-- min/max blocksize
-- min/max framesize
-- sample rate
-- channels
-- bits per sample
-- total samples
-- md5 sum (byte string)

-- we can calculate duration as a floating point number

-- application data (identified by four-byte id, provide enumeration)

-- seektable

-- vorbis comment we need common fields to be pre-defined enumeration to
-- avoid typos

-- cue sheet

-- picture as sequence of bytes, the stuff in docs

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
