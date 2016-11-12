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
  , MetaChainStatus
  , Metadata
  , MetadataType
    -- * Chain
  , chainNew
  , chainStatus
  , chainRead
  , chainMergePadding
  , chainSortPadding
  , chainWrite
  , chainDelete
    -- * Iterator
  , iteratorNew
  , iteratorInit
  , iteratorNext
  , iteratorPrev
  , iteratorGetBlockType
  , iteratorGetBlock
  , iteratorInsertBlockBefore
  , iteratorInsertBlockAfter
  , iteratorDeleteBlock )
where

import Foreign

----------------------------------------------------------------------------
-- Types

newtype MetaChain = MetaChain (Ptr Chain)
newtype MetaIterator = MetaIterator (Ptr Iterator)

data Chain
data Iterator

data MetaChainStatus = MetaChainStatus

data Metadata = Metadata
  { metadataType   :: MetadataType
  , metadataIsLast :: Bool
  , metadataLength :: Word
  }

data MetadataType -- FIXME
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

chainNew :: IO (Maybe MetaChain)
chainNew = undefined -- TODO FLAC__metadata_chain_new

chainStatus :: MetaChain -> IO MetaChainStatus
chainStatus = undefined -- TODO -- FLAC__

chainRead :: MetaChain -> IO Bool
chainRead = undefined -- TODO -- FLAC__metadata_chain_read

chainMergePadding :: MetaChain -> IO () -- use before making iterator
chainMergePadding = undefined -- TODO FLAC__metadata_chain_merge_padding

chainSortPadding :: MetaChain -> IO () -- use before making iterator
chainSortPadding = undefined -- TODO FLAC__metadata_chain_sort_padding

chainWrite :: MetaChain -> Bool -> Bool -> IO Bool
chainWrite = undefined -- TODO FLAC__metadata_chain_write

chainDelete :: MetaChain -> IO ()
chainDelete = undefined -- TODO FLAC__metadata_chain_delete

----------------------------------------------------------------------------
-- Iterator

iteratorNew :: MetaChain -> IO (Maybe MetaIterator)
iteratorNew = undefined -- TODO FLAC__metadata_iterator_new

iteratorInit :: MetaIterator -> MetaChain -> IO ()
iteratorInit = undefined -- TODO FLAC__metadata_iterator_init

iteratorNext :: MetaIterator -> IO Bool
iteratorNext = undefined -- TODO FLAC__metadata_iterator_next

iteratorPrev :: MetaIterator -> IO Bool
iteratorPrev = undefined -- TODO FLAC__metadata_iterator_prev

iteratorGetBlockType :: MetaIterator -> IO MetadataType
iteratorGetBlockType = undefined -- TODO FLAC__metadata_iterator_get_block_type

iteratorGetBlock :: MetaIterator -> IO (Maybe Metadata)
iteratorGetBlock = undefined -- TODO FLAC__metadata_iterator_get_block

iteratorInsertBlockBefore :: MetaIterator -> Metadata -> IO Bool
iteratorInsertBlockBefore = undefined -- TODO FLAC__metadata_iterator_insert_block_before

iteratorInsertBlockAfter :: MetaIterator -> Metadata -> IO Bool
iteratorInsertBlockAfter = undefined -- TODO FLAC__metadata_iterator_insert_block_after

iteratorDeleteBlock :: MetaIterator -> Bool -> IO Bool
iteratorDeleteBlock = undefined -- TODO FLAC__metadata_iterator_delete_block
