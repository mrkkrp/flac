-- |
-- Module      :  Codec.Audio.FLAC.Metadata.Internal.Level2Interface
-- Copyright   :  © 2016 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Low-level Haskell wrapper around C functions to work with level 2 FLAC
-- metadata interface, see:
--
-- <https://xiph.org/flac/api/group__flac__metadata__level2.html>.

{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE ScopedTypeVariables      #-}

module Codec.Audio.FLAC.Metadata.Internal.Level2Interface
  ( -- * Chain
    chainNew
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
  , iteratorGetBlockType
  , iteratorGetBlock
  , iteratorSetBlock
  , iteratorDeleteBlock
  , iteratorInsertBlockBefore
  , iteratorInsertBlockAfter )
where

import Codec.Audio.FLAC.Metadata.Internal.Types
import Codec.Audio.FLAC.Util
import Foreign.C.String
import Foreign.C.Types

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
-- went wrong. Also resets status to 'MetaChainStatusOK'.

chainStatus :: MetaChain -> IO MetaChainStatus
chainStatus = fmap toEnum' . c_chain_status

foreign import ccall unsafe "FLAC__metadata_chain_status"
  c_chain_status :: MetaChain -> IO CUInt

-- | Read all metadata from a FLAC file into the chain. Return 'False' if
-- something went wrong.

chainRead :: MetaChain -> FilePath -> IO Bool
chainRead chain path = withCString path (c_chain_read chain)

foreign import ccall unsafe "FLAC__metadata_chain_read"
  c_chain_read :: MetaChain -> CString -> IO Bool

-- | Write all metadata out to the FLAC file.

chainWrite
  :: MetaChain         -- ^ The chain to write
  -> Bool              -- ^ Whether to use padding
  -> Bool              -- ^ Whether to preserve file stats
  -> IO Bool           -- ^ 'False' if something went wrong
chainWrite chain usePadding preserveStats =
  c_chain_write chain (fromEnum' usePadding) (fromEnum' preserveStats)

foreign import ccall unsafe "FLAC__metadata_chain_write"
  c_chain_write :: MetaChain -> CInt -> CInt -> IO Bool

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
iteratorNext = c_iterator_next

foreign import ccall unsafe "FLAC__metadata_iterator_next"
  c_iterator_next :: MetaIterator -> IO Bool

-- | Move the iterator backward one metadata block, returning 'False' if
-- already at the beginning.

iteratorPrev :: MetaIterator -> IO Bool
iteratorPrev = c_iterator_prev

foreign import ccall unsafe "FLAC__metadata_iterator_prev"
  c_iterator_prev :: MetaIterator -> IO Bool

-- | Get the type of the metadata block at the current position. Useful for
-- fast searching.

iteratorGetBlockType :: MetaIterator -> IO MetadataType
iteratorGetBlockType = fmap toEnum' . c_iterator_get_block_type

foreign import ccall unsafe "FLAC__metadata_iterator_get_block_type"
  c_iterator_get_block_type :: MetaIterator -> IO CUInt

-- | Get metadata block at the current position.

iteratorGetBlock :: MetaIterator -> IO Metadata
iteratorGetBlock = c_iterator_get_block

foreign import ccall unsafe "FLAC__metadata_iterator_get_block"
  c_iterator_get_block :: MetaIterator -> IO Metadata

-- | Write given 'Metadata' block at position pointed to by 'MetaIterator'
-- replacing existing block.

iteratorSetBlock :: MetaIterator -> Metadata -> IO Bool
iteratorSetBlock = c_iterator_set_block

foreign import ccall unsafe "FLAC__metadata_iterator_set_block"
  c_iterator_set_block :: MetaIterator -> Metadata -> IO Bool

-- | Remove the current block from the chain.

iteratorDeleteBlock
  :: MetaIterator      -- ^ Iterator that determines the position
  -> Bool              -- ^ Whether to replace the block with padding
  -> IO Bool           -- ^ 'False' if something went wrong
iteratorDeleteBlock = c_iterator_delete_block

foreign import ccall unsafe "FLAC__metadata_iterator_delete_block"
  c_iterator_delete_block :: MetaIterator -> Bool -> IO Bool

-- | Insert a new 'Metadata' block before the current block. You cannot
-- insert a block before the first 'StreamInfo' block. You cannot insert a
-- 'StreamInfo' block as there can be only one, the one that already exists
-- at the head when you read in a chain. The chain takes ownership of the
-- new block and it will be deleted when the chain is deleted. The iterator
-- will be left pointing to the new block.
--
-- The function returns 'False' if something went wrong.

iteratorInsertBlockBefore :: MetaIterator -> Metadata -> IO Bool
iteratorInsertBlockBefore = c_iterator_insert_block_before

foreign import ccall unsafe "FLAC__metadata_iterator_insert_block_before"
  c_iterator_insert_block_before :: MetaIterator -> Metadata -> IO Bool

-- | Insert a new block after the current block. You cannot insert a
-- 'StreamInfo' block as there can be only one, the one that already exists
-- at the head when you read in a chain. The chain takes ownership of the
-- new block and it will be deleted when the chain is deleted. The iterator
-- will be left pointing to the new block.
--
-- The function returns 'False' if something went wrong.

iteratorInsertBlockAfter :: MetaIterator -> Metadata -> IO Bool
iteratorInsertBlockAfter = c_iterator_insert_block_after

foreign import ccall unsafe "FLAC__metadata_iterator_insert_block_after"
  c_iterator_insert_block_after :: MetaIterator -> Metadata -> IO Bool
