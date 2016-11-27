--
-- Test suite for metadata manipulation.
--
-- Copyright © 2016 Mark Karpov <markkarpov@openmailbox.org>
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are
-- met:
--
-- * Redistributions of source code must retain the above copyright notice,
--   this list of conditions and the following disclaimer.
--
-- * Redistributions in binary form must reproduce the above copyright
--   notice, this list of conditions and the following disclaimer in the
--   documentation and/or other materials provided with the distribution.
--
-- * Neither the name Mark Karpov nor the names of contributors may be used
--   to endorse or promote products derived from this software without
--   specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS “AS IS” AND ANY
-- EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
-- WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
-- DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS BE LIABLE FOR ANY
-- DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
-- OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
-- HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
-- STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
-- ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
-- POSSIBILITY OF SUCH DAMAGE.

{-# LANGUAGE OverloadedStrings #-}

module Codec.Audio.FLAC.MetadataSpec
  ( spec )
where

import Codec.Audio.FLAC.Metadata hiding (runFlacMeta)
import Control.Monad.IO.Class (MonadIO (..))
import Data.ByteString (ByteString)
import Data.Default.Class
import Data.List.NonEmpty (NonEmpty (..))
import System.Directory
import System.IO.Temp (withSystemTempFile)
import Test.Hspec
import qualified Codec.Audio.FLAC.Metadata as Flac
import qualified Data.ByteString as B

spec :: Spec
spec = around withSandbox $ do

  describe "MinBlockSize" $
    it "is read correctly" $ \path ->
      runFlacMeta def path $ do
        v <- retrieve MinBlockSize
        liftIO (v `shouldBe` 4096)
        chainIntact

  describe "MaxBlockSize" $
    it "is read correctly" $ \path ->
      runFlacMeta def path $ do
        v <- retrieve MaxBlockSize
        liftIO (v `shouldBe` 4096)
        chainIntact

  describe "MinFrameSize" $
    it "is read correctly" $ \path ->
      runFlacMeta def path $ do
        v <- retrieve MinFrameSize
        liftIO (v `shouldBe` 1270)
        chainIntact

  describe "MaxFrameSize" $
    it "is read correctly" $ \path ->
      runFlacMeta def path $ do
        v <- retrieve MaxFrameSize
        liftIO (v `shouldBe` 2504)
        chainIntact

  describe "SampleRate" $
    it "is read correctly" $ \path ->
      runFlacMeta def path $ do
        v <- retrieve SampleRate
        liftIO (v `shouldBe` 44100)
        chainIntact

  describe "SampleRate" $
    it "is read correctly" $ \path ->
      runFlacMeta def path $ do
        v <- retrieve Channels
        liftIO (v `shouldBe` 2)
        chainIntact

  describe "BitsPerSample" $
    it "is read correctly" $ \path ->
      runFlacMeta def path $ do
        v <- retrieve BitsPerSample
        liftIO (v `shouldBe` 16)
        chainIntact

  describe "TotalSamples" $
    it "is read correctly" $ \path ->
      runFlacMeta def path $ do
        v <- retrieve TotalSamples
        liftIO (v `shouldBe` 18304)
        chainIntact

  describe "FileSize" $
    it "is read correctly" $ \path ->
      runFlacMeta def path $ do
        v <- retrieve FileSize
        liftIO (v `shouldBe` 11459)
        chainIntact

  describe "BitRate" $
    it "is read correctly" $ \path ->
      runFlacMeta def path $ do
        v <- retrieve BitRate
        liftIO (v `shouldBe` 220)
        chainIntact

  describe "MD5Sum" $
    it "is read correctly" $ \path ->
      runFlacMeta def path $ do
        v <- retrieve MD5Sum
        liftIO (v `shouldBe` refMD5Sum)
        chainIntact

  describe "Duration" $
    it "is read correctly" $ \path ->
      runFlacMeta def path $ do
        v <- retrieve Duration
        liftIO (v `shouldBe` 0.41505668934240364)
        chainIntact

----------------------------------------------------------------------------
-- Helpers

-- | Type constrained version of 'Flac.runFlacMeta' to remove type
-- ambiguity.

runFlacMeta :: FlacMetaSettings -> FilePath -> FlacMeta a -> IO a
runFlacMeta = Flac.runFlacMeta

-- | Make a temporary copy of @audio-samples/sample.flac@ file and provide
-- the path to the file. Automatically remove the file when the test
-- finishes.

withSandbox :: ActionWith FilePath -> IO ()
withSandbox action = withSystemTempFile "sample.flac" $ \path _ -> do
  copyFile "audio-samples/sample.flac" path
  action path

-- | Check that the meta chain is intact.

chainIntact :: FlacMeta ()
chainIntact = do
  chain <- getMetaChain
  liftIO (chain `shouldBe` refChain)
  modified <- isMetaChainModified
  liftIO (modified `shouldBe` False)

-- | MD5 sum of uncompressed audio data of the unmodified sample we use in
-- these tests.

refMD5Sum :: ByteString
refMD5Sum = B.pack [89,191,106,236,125,27,65,161,78,138,172,153,91,60,42,109]

-- | The sequence of metadata blocks as they appear in the unmodified sample
-- we use in these tests.

refChain :: NonEmpty MetadataType
refChain = StreamInfoBlock :| [VorbisCommentBlock, PaddingBlock]
