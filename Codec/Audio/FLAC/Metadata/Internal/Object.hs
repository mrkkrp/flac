-- |
-- Module      :  Codec.Audio.FLAC.Metadata.Internal.Object
-- Copyright   :  © 2016 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Wrappers for functions to work with metadata objects like Vorbis
-- comments.

module Codec.Audio.FLAC.Metadata.Internal.Object
  (  )
where

----------------------------------------------------------------------------
-- Inspecting and setting matadata blocks

-- view :: forall a. Storable a => MetaIterator -> Int -> Proxy a -> IO a
-- view iter offset Proxy = do
--   ptr <- c_iterator_get_block iter
--   peek (alignPtr (ptr `plusPtr` offset) (alignment (undefined :: a))) -- think more about this

-- viewBA :: MetaIterator -> Int -> Int -> IO ByteString
-- viewBA iter offset size = do
--   ptr <- c_iterator_get_block iter
--   B.pack <$> peekArray size (ptr `plusPtr` offset)

-- data Vorbis = Vorbis
--   { vorbisVendor   :: Text
--   , vorbisComments :: HashMap Text Text
--   } deriving (Show, Read, Eq)

--  If decoding fails, a UnicodeException is thrown.
-- this perhaps should be incapsulated in a Storable instance

-- viewVorbis :: MetaIterator -> IO Vorbis
-- viewVorbis iter = do
--   ptr       <- c_iterator_get_block iter
--   let headerSize = sizeOf (0 :: CUInt) * 3
--       ptrSize = sizeOf (undefined :: Ptr Void)
--   vendor    <- peekVorbisStr (ptr `plusPtr` headerSize)
  -- let vorbisOffset = 4 + ptrSize
  -- numComments <- peekByteOff ptr (headerSize + vorbisOffset) :: IO Word32
  -- print numComments
  -- let go :: Word32 -> Ptr a -> HashMap Text Text -> IO (HashMap Text Text)
  --     go n ptr m = do
  --       let commentOffset = headerSize + vorbisOffset + 4 + ptrSize * fromIntegral n
  --       commentPtr <- peek (ptr `plusPtr` commentOffset)
  --       comment    <- peekVorbisStr commentPtr
  --       let (name, value) = second (T.drop 1) (T.breakOn "=" comment)
  --           m' = HM.insert name value m
  --       if n == 0
  --         then return m'
  --         else go (n - 1) m'
  -- comments <- go numComments HM.empty
  -- return Vorbis
  --   { vorbisVendor   = vendor
  --   , vorbisComments = HM.empty }

-- peekVorbisStr :: Ptr a -> IO Text
-- peekVorbisStr ptr = do
--   len  <- peekByteOff ptr 0 :: IO Word32
--   let strPtr = alignPtr (ptr `plusPtr` 4) (alignment (undefined :: Ptr Void))
--   T.peekCStringLen (strPtr, fromIntegral len)

-- constructVorbis :: Vorbis -> IO (Ptr Metadata)
-- constructVorbis = undefined
