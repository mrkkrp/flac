{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      :  Codec.Audio.FLAC.Util
-- Copyright   :  © 2016–present Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Random non-public helpers.
module Codec.Audio.FLAC.Util
  ( maybePtr,
    toEnum',
    fromEnum',
    peekCStringText,
    withCStringText,
    withTempFile',
  )
where

import Control.Exception
import Data.ByteString qualified as B
import Data.Coerce
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Foreign
import Foreign.C.String
import System.Directory
import System.FilePath
import System.IO

-- | Coerce to 'Ptr' and check if it's a null pointer, return 'Nothing' if
-- it is, otherwise return the given pointer unchanged. Needless to say that
-- this is unsafe.
maybePtr :: (Coercible a (Ptr p)) => a -> Maybe a
maybePtr a
  | coerce a == nullPtr = Nothing
  | otherwise = Just a

-- | A version of 'toEnum' that converts from any 'Integral' type.
toEnum' :: (Integral a, Enum b) => a -> b
toEnum' = toEnum . fromIntegral

-- | A version of 'fromEnum' that is polymorphic in return type.
fromEnum' :: (Integral a, Enum b) => b -> a
fromEnum' = fromIntegral . fromEnum

-- | Peek 'CString' and decode it as UTF-8 encoded value.
peekCStringText :: CString -> IO Text
peekCStringText = fmap T.decodeUtf8 . B.packCString

-- | Convert a 'Text' value to a null-terminated C string that will be freed
-- automatically. Null bytes are removed from the 'Text' value first.
withCStringText :: Text -> (CString -> IO a) -> IO a
withCStringText text = B.useAsCString bytes
  where
    bytes = T.encodeUtf8 (T.filter (/= '\0') text)

-- | A custom wrapper for creating temporary files in the same directory as
-- the given file. 'Handle' is not opened, you only get 'FilePath' in the
-- callback.
withTempFile' :: FilePath -> (FilePath -> IO a) -> IO a
withTempFile' path m = bracketOnError acquire cleanup $
  \(path', h) -> hClose h >> m path'
  where
    acquire = openBinaryTempFile dir file
    -- NOTE We need ignoringIOErrors in the case exception strikes before we
    -- create the actual file.
    cleanup = ignoringIOErrors . removeFile . fst
    dir = takeDirectory path
    file = takeFileName path

-- | Perform the specified action ignoring IO exceptions it may throw.
ignoringIOErrors :: IO () -> IO ()
ignoringIOErrors ioe = ioe `catch` handler
  where
    handler :: IOError -> IO ()
    handler = const (return ())
