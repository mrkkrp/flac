-- |
-- Module      :  Codec.Audio.FLAC.Metadata.Internal
-- Copyright   :  © 2016 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Low-level Haskell wrapper around C functions to work with FLAC metadata.

{-# LANGUAGE ForeignFunctionInterface #-}

module Codec.Audio.FLAC.Metadata.Internal
  ( -- * Types
    MetaChain
  , MetaIterator
  , MetaChainStatus (..)
  , Metadata
  , MetadataData
    -- * Chain
  , chainNew
  , chainDelete
  , chainStatus
  , chainRead
  , chainWrite
  , chainSortPadding
    -- * Iterator
  , iteratorNew
  , iteratorDelete
  , iteratorInit
  , iteratorNext
  , iteratorPrev
  -- , iteratorGetBlockType
  , iteratorGetBlock
  , iteratorDeleteBlock
  , iteratorInsertBlockBefore
  , iteratorInsertBlockAfter )
where

import Data.Void
import Foreign
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Unsafe.Coerce

----------------------------------------------------------------------------
-- Types

newtype MetaChain = MetaChain (Ptr Void) -- there is no life in Void, only death
newtype MetaIterator = MetaIterator (Ptr Void)

-- | Enumeration of statuses of 'MetaChain'.

data MetaChainStatus
  = MetaChainStatusOK
    -- ^ The chain is in the normal OK state
  | MetaChainStatusIllegalInput
    -- ^ The data passed into a function violated the function's usage
    -- criteria
  | MetaChainStatusErrorOpeningFile
    -- ^ The chain could not open the target file
  | MetaChainStatusNotFlacFile
    -- ^ The chain could not find the FLAC signature at the start of the
    -- file
  | MetaChainStatusNotWritable
    -- ^ The chain tried to write to a file that was not writable
  | MetaChainStatusBadMetadata
    -- ^ The chain encountered input that does not conform to the FLAC
    -- metadata specification
  | MetaChainStatusReadError
    -- ^ The chain encountered an error while reading the FLAC file
  | MetaChainStatusSeekError
    -- ^ The chain encountered an error while seeking in the FLAC file
  | MetaChainStatusWriteError
    -- ^ The chain encountered an error while writing the FLAC file
  | MetaChainStatusRenameError
    -- ^ The chain encountered an error renaming the FLAC file
  | MetaChainStatusUnlinkError
    -- ^ The chain encountered an error removing the temporary file
  | MetaChainStatusMemoryAllocationError
    -- ^ Memory allocation failed
  | MetaChainStatusInternalError
    -- ^ The caller violated an assertion or an unexpected error occurred
  | MetaChainStatusInvalidCallbacks
    -- ^ One or more of the required callbacks was NULL
  | MetaChainStatusReadWriteMismatch
    -- ^ This error occurs when read and write methods do not match (i.e.
    -- when if you read with callbacks, you should also write with
    -- callbacks).
  | MetaChainStatusWrongWriteCall
    -- ^ Should not ever happen when you use this binding.
  deriving (Eq, Ord, Bounded, Enum)

data Metadata = Metadata
  { metadataIsLast :: Bool
  , metadataLength :: Word
  , metadataData   :: MetadataData
  }

data MetadataData -- FIXME
  = StreamInfo
  | Padding
  | Application
  | SeekTable
  | VorbisComment
  | CueSheet
  | Picture
  | Unknown

----------------------------------------------------------------------------
-- Chain

-- | Create a new 'MetaChain'. In the case of memory allocation problem
-- 'Nothing' is returned.

chainNew :: IO (Maybe MetaChain)
chainNew = maybePtr <$> c_chain_new

foreign import ccall unsafe "FLAC__metadata_chain_new"
  c_chain_new :: IO MetaChain

-- | Free a chain instance. Deletes the object pointed to by chain.

chainDelete :: MetaChain -> IO ()
chainDelete = c_chain_delete

foreign import ccall unsafe "FLAC__metadata_chain_delete"
  c_chain_delete :: MetaChain -> IO ()

-- | Check status of given 'MetaChain'. This can be used to find out what
-- went wrong. Also resents status to 'MetaChainStatusOK'.

chainStatus :: MetaChain -> IO MetaChainStatus
chainStatus = fmap toEnum' . c_chain_status

foreign import ccall unsafe "FLAC__metadata_chain_status"
  c_chain_status :: MetaChain -> IO CUInt

-- | Read all metadata from a FLAC file into the chain. Return 'False' if
-- something went wrong.

chainRead :: MetaChain -> FilePath -> IO Bool
chainRead chain path =
  withCString path $ \cstr ->
    toEnum' <$> c_chain_read chain cstr

foreign import ccall unsafe "FLAC__metadata_chain_read"
  c_chain_read :: MetaChain -> CString -> IO CInt

-- | Write all metadata out to the FLAC file.

chainWrite
  :: MetaChain         -- ^ The chain to write
  -> Bool              -- ^ Whether to use padding
  -> Bool              -- ^ Whether to preserve file stats
  -> IO Bool           -- ^ 'False' if something went wrong
chainWrite chain usePadding preserveStats = toEnum' <$>
  c_chain_write chain (fromEnum' usePadding) (fromEnum' preserveStats)

foreign import ccall unsafe "FLAC__metadata_chain_write"
  c_chain_write :: MetaChain -> CInt -> CInt -> IO CInt

-- | Move all padding blocks to the end on the metadata, then merge them
-- into a single block. Useful to get maximum padding to have better changes
-- for re-writing only metadata blocks, not entire FLAC file. Any iterator
-- on the current chain will become invalid after this call. You should
-- delete the iterator and get a new one.
--
-- Note: this function does not write to the FLAC file, it only modifies the
-- chain.

chainSortPadding :: MetaChain -> IO ()
chainSortPadding = c_chain_sort_padding

foreign import ccall unsafe "FLAC__metadata_chain_sort_padding"
  c_chain_sort_padding :: MetaChain -> IO ()

----------------------------------------------------------------------------
-- Iterator

-- | Create a new iterator. Return 'Nothing' if there was a problem with
-- memory allocation.

iteratorNew :: IO (Maybe MetaIterator)
iteratorNew = maybePtr <$> c_iterator_new

foreign import ccall unsafe "FLAC__metadata_iterator_new"
  c_iterator_new :: IO MetaIterator

-- | Free an iterator instance. Deletes the object pointed to by
-- 'MetaIterator'.

iteratorDelete :: MetaIterator -> IO ()
iteratorDelete = c_iterator_delete

foreign import ccall unsafe "FLAC__metadata_iterator_delete"
  c_iterator_delete :: MetaIterator -> IO ()

-- | Initialize the iterator to point to the first metadata block in the
-- given chain.

iteratorInit
  :: MetaIterator      -- ^ Existing iterator
  -> MetaChain         -- ^ Existing initialized chain
  -> IO ()
iteratorInit = c_iterator_init

foreign import ccall unsafe "FLAC__metadata_iterator_init"
  c_iterator_init :: MetaIterator -> MetaChain -> IO ()

-- | Move the iterator forward one metadata block, returning 'False' if
-- already at the end.

iteratorNext :: MetaIterator -> IO Bool
iteratorNext = fmap toEnum' . c_iterator_next

foreign import ccall unsafe "FLAC__metadata_iterator_next"
  c_iterator_next :: MetaIterator -> IO CInt

-- | Move the iterator backward one metadata block, returning 'False' if
-- already at the beginning.

iteratorPrev :: MetaIterator -> IO Bool
iteratorPrev = fmap toEnum' . c_iterator_prev

foreign import ccall unsafe "FLAC__metadata_iterator_prev"
  c_iterator_prev :: MetaIterator -> IO CInt

-- iteratorGetBlockType :: MetaIterator -> IO MetadataType
-- iteratorGetBlockType = undefined -- TODO FLAC__metadata_iterator_get_block_type

iteratorGetBlock :: MetaIterator -> IO (Maybe Metadata)
iteratorGetBlock = undefined -- TODO FLAC__metadata_iterator_get_block

-- | Remove the current block from the chain.

iteratorDeleteBlock
  :: MetaIterator      -- ^ Iterator that determines the position
  -> Bool              -- ^ Whether to replace the block with padding
  -> IO Bool           -- ^ 'False' if something went wrong
iteratorDeleteBlock iterator replaceWithPadding = toEnum' <$>
  c_iterator_delete_block iterator (fromEnum' replaceWithPadding)

foreign import ccall unsafe "FLAC__metadata_iterator_delete_block"
  c_iterator_delete_block :: MetaIterator -> CInt -> IO CInt

iteratorInsertBlockBefore :: MetaIterator -> Metadata -> IO Bool
iteratorInsertBlockBefore = undefined -- TODO FLAC__metadata_iterator_insert_block_before

iteratorInsertBlockAfter :: MetaIterator -> Metadata -> IO Bool
iteratorInsertBlockAfter = undefined -- TODO FLAC__metadata_iterator_insert_block_after

----------------------------------------------------------------------------
-- Helpers

-- | Coerce to 'Ptr' and check if it's a null pointer, return 'Nothing' if
-- it is, otherwise return the given pointer unchanged.

maybePtr :: a -> Maybe a
maybePtr ptr
  | unsafeCoerce ptr == nullPtr = Nothing
  | otherwise                   = Just ptr

-- | A version of 'toEnum' that converts from any 'Integral' type.

toEnum' :: (Integral a, Enum b) => a -> b
toEnum' = toEnum . fromIntegral

-- | A version of 'fromEnum' that is polymorphic in return type.

fromEnum' :: (Integral a, Enum b) => b -> a
fromEnum' = fromIntegral . fromEnum
