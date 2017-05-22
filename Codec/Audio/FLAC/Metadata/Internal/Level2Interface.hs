-- |
-- Module      :  Codec.Audio.FLAC.Metadata.Internal.Level2Interface
-- Copyright   :  © 2016–2017 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Low-level Haskell wrapper around C functions to work with level 2 FLAC
-- metadata interface, see:
--
-- <https://xiph.org/flac/api/group__flac__metadata__level2.html>.

{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Codec.Audio.FLAC.Metadata.Internal.Level2Interface
  ( -- * Chain
    withChain
  , chainStatus
  , chainRead
  , chainWrite
  , chainSortPadding
    -- * Iterator
  , withIterator
  , iteratorGetBlockType
  , iteratorGetBlock
  , iteratorSetBlock
  , iteratorDeleteBlock
  , iteratorInsertBlockAfter )
where

import Codec.Audio.FLAC.Metadata.Internal.Types
import Codec.Audio.FLAC.Util
import Control.Monad.Catch
import Control.Monad.IO.Class (MonadIO (..))
import Foreign.C.String
import Foreign.C.Types
import Prelude hiding (iterate)

----------------------------------------------------------------------------
-- Chain

-- | Create and use a 'MetaChain' (metadata chain). The chain is guaranteed
-- to be freed even in case of exception.
--
-- If memory for the chain cannot be allocated, corresponding
-- 'MetaException' is raised.

withChain :: (MetaChain -> IO a) -> IO a
withChain f = bracket chainNew (mapM_ chainDelete) $ \mchain ->
  case mchain of
    Nothing -> throwM
      (MetaGeneralProblem MetaChainStatusMemoryAllocationError)
    Just x -> f x

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

-- | Traverse all metadata blocks from beginning to end collecting 'Just'
-- values and possibly performing some actions. This is the only way to
-- traverse metadata chain and get access to 'MetaIterator' and by exporting
-- only this, we eliminate certain class of possible errors making finding
-- and traversing metadata blocks always correct and safe.
--
-- If memory for the iterator cannot be allocated, corresponding
-- 'MetaException' is raised.

withIterator :: (MonadMask m, MonadIO m)
  => MetaChain         -- ^ Metadata chain to traverse
  -> (MetaIterator -> m (Maybe a)) -- ^ Action to perform on each block
  -> m [a]             -- ^ Accumulated results
withIterator chain f = bracket acquire release action
  where
    acquire = liftIO iteratorNew
    release = mapM_ (liftIO . iteratorDelete)
    action mi =
      case mi of
        Nothing -> throwM
          (MetaGeneralProblem MetaChainStatusMemoryAllocationError)
        Just i -> do
          liftIO (iteratorInit i chain)
          let go thisNext =
                if thisNext
                  then do
                    res <- f i
                    let next = liftIO (iteratorNext i) >>= go
                    case res of
                      Nothing -> next
                      Just  x -> (x :) <$> next
                  else return []
          go True

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
  -> IO Bool           -- ^ 'False' if something went wrong
iteratorDeleteBlock block = c_iterator_delete_block block False

foreign import ccall unsafe "FLAC__metadata_iterator_delete_block"
  c_iterator_delete_block :: MetaIterator -> Bool -> IO Bool

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
