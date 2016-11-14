-- |
-- Module      :  Codec.Audio.FLAC.Util
-- Copyright   :  © 2016 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Low-level utilities.

module Codec.Audio.FLAC.Util
  ( maybePtr
  , toEnum'
  , fromEnum' )
where

import Foreign.Ptr
import Unsafe.Coerce

----------------------------------------------------------------------------
-- Helpers

-- | Coerce to 'Ptr' and check if it's a null pointer, return 'Nothing' if
-- it is, otherwise return the given pointer unchanged.

maybePtr :: a -> Maybe a
maybePtr a
  | unsafeCoerce a == nullPtr = Nothing
  | otherwise                 = Just a

-- | A version of 'toEnum' that converts from any 'Integral' type.

toEnum' :: (Integral a, Enum b) => a -> b
toEnum' = toEnum . fromIntegral

-- | A version of 'fromEnum' that is polymorphic in return type.

fromEnum' :: (Integral a, Enum b) => b -> a
fromEnum' = fromIntegral . fromEnum
