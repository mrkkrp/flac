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
import Control.Monad
import Control.Monad.IO.Class (MonadIO (..))
import Data.ByteString (ByteString)
import Data.Default.Class
import Data.List.NonEmpty (NonEmpty (..))
import Data.Vector (Vector)
import System.Directory
import System.IO.Temp (withSystemTempFile)
import Test.Hspec hiding (shouldBe, shouldReturn)
import qualified Codec.Audio.FLAC.Metadata as Flac
import qualified Data.ByteString           as B
import qualified Data.Vector               as V
import qualified Test.Hspec                as Hspec

-- TODO How to share the same sandbox between several subsequent tests? This
-- would allow for more precise labelling.

spec :: Spec
spec = around withSandbox $ do
  describe "MinBlockSize" $
    it "is read correctly" $ \path ->
      runFlacMeta def path . checkNoMod $
        retrieve MinBlockSize `shouldReturn` 4096

  describe "MaxBlockSize" $
    it "is read correctly" $ \path ->
      runFlacMeta def path . checkNoMod $
        retrieve MaxBlockSize `shouldReturn` 4096

  describe "MinFrameSize" $
    it "is read correctly" $ \path ->
      runFlacMeta def path . checkNoMod $
        retrieve MinFrameSize `shouldReturn` 1270

  describe "MaxFrameSize" $
    it "is read correctly" $ \path ->
      runFlacMeta def path . checkNoMod $
        retrieve MaxFrameSize `shouldReturn` 2504

  describe "SampleRate" $
    it "is read correctly" $ \path ->
      runFlacMeta def path . checkNoMod $
        retrieve SampleRate `shouldReturn` 44100

  describe "SampleRate" $
    it "is read correctly" $ \path ->
      runFlacMeta def path . checkNoMod $
        retrieve Channels `shouldReturn` 2

  describe "BitsPerSample" $
    it "is read correctly" $ \path ->
      runFlacMeta def path . checkNoMod $
        retrieve BitsPerSample `shouldReturn` 16

  describe "TotalSamples" $
    it "is read correctly" $ \path ->
      runFlacMeta def path . checkNoMod $
        retrieve TotalSamples `shouldReturn` 18304

  describe "FileSize" $
    it "is read correctly" $ \path ->
      runFlacMeta def path . checkNoMod $
        retrieve FileSize `shouldReturn` 11459

  describe "BitRate" $
    it "is read correctly" $ \path ->
      runFlacMeta def path . checkNoMod $
        retrieve BitRate `shouldReturn` 220

  describe "MD5Sum" $
    it "is read correctly" $ \path ->
      runFlacMeta def path . checkNoMod $
        retrieve MD5Sum `shouldReturn` refMD5Sum

  describe "Duration" $
    it "is read correctly" $ \path ->
      runFlacMeta def path . checkNoMod $
        retrieve Duration `shouldReturn` 0.41505668934240364

  describe "Application" $
    it "is set/read/deleted correctly" $ \path -> do
      -- Can set application data.
      runFlacMeta def path $ do
        Application "foo"  =-> Just "foo"
        Application "bobo" =-> Just "bobo"
        getMetaChain `shouldReturn` StreamInfoBlock :|
          [ApplicationBlock,ApplicationBlock,VorbisCommentBlock,PaddingBlock]
        isMetaChainModified `shouldReturn` True
      -- Can read it back.
      runFlacMeta def path . checkNoMod $ do
        retrieve (Application "foo")  `shouldReturn` Just "foo"
        retrieve (Application "bobo") `shouldReturn` Just "bobo"
      -- Can wipe one without affecting the other.
      runFlacMeta def path $ do
        Application "foo" =-> Nothing
        retrieve (Application "foo")  `shouldReturn` Nothing
        retrieve (Application "bobo") `shouldReturn` Just "bobo"
        getMetaChain `shouldReturn` StreamInfoBlock :|
          [ApplicationBlock,VorbisCommentBlock,PaddingBlock]
        isMetaChainModified `shouldReturn` True
      -- Can overwrite application data.
      runFlacMeta def path $ do
        Application "bobo" =-> Just "moon"
        retrieve (Application "bobo") `shouldReturn` Just "moon"
        getMetaChain `shouldReturn` StreamInfoBlock :|
          [ApplicationBlock,VorbisCommentBlock,PaddingBlock]
        isMetaChainModified `shouldReturn` True
      -- Can wipe the other one bringing it to the default state.
      runFlacMeta def path $ do
        Application "bobo" =-> Nothing
        getMetaChain `shouldReturn` refChain
        isMetaChainModified `shouldReturn` True
      runFlacMeta def path . checkNoMod $ do
        retrieve (Application "foo")  `shouldReturn` Nothing
        retrieve (Application "bobo") `shouldReturn` Nothing

  describe "SeekTable" $ do
    it "raises exception when invalid seek table given" $
      const pending
      -- FIXME Invalid seek tables raise exceptions.
      -- let m = runFlacMeta def path $
      --       SeekTable =-> Just invalidSeekTable
      -- m `shouldThrow` (== FlacMetaInvalidSeekTable)
    it "is set/read/deleted correctly" $ \path -> do
      -- Can set seek table if it's correct.
      runFlacMeta def path $ do
        SeekTable =-> Just testSeekTable
        getMetaChain `shouldReturn` StreamInfoBlock :|
          [SeekTableBlock,VorbisCommentBlock,PaddingBlock]
        isMetaChainModified `shouldReturn` True
      -- Can read it back.
      runFlacMeta def path . checkNoMod $
        retrieve SeekTable `shouldReturn` Just testSeekTable
      -- Can delete it.
      runFlacMeta def path $ do
        SeekTable =-> Nothing
        getMetaChain `shouldReturn` StreamInfoBlock :|
          [VorbisCommentBlock,PaddingBlock]
        isMetaChainModified `shouldReturn` True
    context "when auto-vacuum disabled" $
      it "can write empty seek table" $ \path -> do
        runFlacMeta def { flacMetaAutoVacuum = False } path $ do
          SeekTable =-> Just V.empty
          retrieve SeekTable `shouldReturn` Just V.empty
          getMetaChain `shouldReturn` StreamInfoBlock :|
            [SeekTableBlock,VorbisCommentBlock,PaddingBlock]
          isMetaChainModified `shouldReturn` True
        runFlacMeta def path . checkNoMod $
          retrieve SeekTable `shouldReturn` Just V.empty
    context "when auto-vacuum enabled" $
      it "empty seek table is removed automatically" $ \path -> do
        runFlacMeta def { flacMetaAutoVacuum = True } path $ do
          SeekTable =-> Just V.empty
          retrieve SeekTable `shouldReturn` Just V.empty
          getMetaChain `shouldReturn` StreamInfoBlock :|
            [SeekTableBlock,VorbisCommentBlock,PaddingBlock]
          isMetaChainModified `shouldReturn` True
        runFlacMeta def path . checkNoMod $ do
          retrieve SeekTable `shouldReturn` Nothing
          getMetaChain `shouldReturn` StreamInfoBlock :|
            [VorbisCommentBlock,PaddingBlock]

  describe "VorbisVendor" $ do
    it "is set/read correctly" $ \path -> do
      -- Can set vorbis vendor.
      runFlacMeta def path $ do
        VorbisVendor =-> Just "foo"
        getMetaChain `shouldReturn` StreamInfoBlock :|
          [VorbisCommentBlock,PaddingBlock]
        isMetaChainModified `shouldReturn` True
      -- Can read it back.
      runFlacMeta def path . checkNoMod $
        retrieve VorbisVendor `shouldReturn` Just "foo"
    context "when auto-vacuum disabled" $
      it "deletion just sets the field to empty string" $ \path -> do
        runFlacMeta def { flacMetaAutoVacuum = False } path $ do
          VorbisVendor =-> Nothing
          retrieve VorbisVendor `shouldReturn` Just ""
          getMetaChain `shouldReturn` StreamInfoBlock :|
            [VorbisCommentBlock,PaddingBlock]
          isMetaChainModified `shouldReturn` True
        runFlacMeta def path . checkNoMod $
          retrieve VorbisVendor `shouldReturn` Just ""
    context "when auto-vacuum enabled" $ do
      context "when no other vorbis fields set" $
        it "empty vendor causes removal of vorbis vendor block" $ \path -> do
          runFlacMeta def { flacMetaAutoVacuum = True } path $ do
            VorbisVendor =-> Nothing
            retrieve VorbisVendor `shouldReturn` Just ""
            getMetaChain `shouldReturn` StreamInfoBlock :|
              [VorbisCommentBlock,PaddingBlock]
            isMetaChainModified `shouldReturn` True
          runFlacMeta def path . checkNoMod $ do
            retrieve VorbisVendor `shouldReturn` Nothing
            getMetaChain `shouldReturn` StreamInfoBlock :| [PaddingBlock]
      context "when other vorbis fields exist" $
        it "deletion just sets the field to empty string" $ \path -> do
          runFlacMeta def { flacMetaAutoVacuum = True } path $ do
            VorbisComment Title =-> Just "bobla"
            VorbisVendor =-> Nothing
            retrieve VorbisVendor `shouldReturn` Just ""
            getMetaChain `shouldReturn` StreamInfoBlock :|
              [VorbisCommentBlock,PaddingBlock]
            isMetaChainModified `shouldReturn` True
          runFlacMeta def path . checkNoMod $ do
            retrieve VorbisVendor `shouldReturn` Just ""
            getMetaChain `shouldReturn` StreamInfoBlock :|
              [VorbisCommentBlock,PaddingBlock]

  describe "VorbisComment" . forM_ [minBound..maxBound] $ \vfield ->
    it (show vfield ++ " is set/read/deleted correctly") $ \path -> do
      -- Can set vorbis comment.
      runFlacMeta def path $ do
        VorbisComment vfield =-> Just "foo"
        getMetaChain `shouldReturn` StreamInfoBlock :|
          [VorbisCommentBlock,PaddingBlock]
        isMetaChainModified `shouldReturn` True
      -- Can read it back.
      runFlacMeta def path . checkNoMod $
        retrieve (VorbisComment vfield) `shouldReturn` Just "foo"
      -- Can delete it.
      runFlacMeta def path $ do
        VorbisComment vfield =-> Nothing
        retrieve (VorbisComment vfield) `shouldReturn` Nothing
        getMetaChain `shouldReturn` StreamInfoBlock :|
          [VorbisCommentBlock,PaddingBlock]
        isMetaChainModified `shouldReturn` True
      runFlacMeta def path . checkNoMod $
        retrieve (VorbisComment vfield) `shouldReturn` Nothing

----------------------------------------------------------------------------
-- Helpers

infix 1 `shouldBe`, `shouldReturn`

-- | Lifted 'Hspec.shouldBe'.

shouldBe :: (MonadIO m, Show a, Eq a) => a -> a -> m ()
shouldBe x y = liftIO (x `Hspec.shouldBe` y)

-- | Lifted 'Hspec.shouldReturn'.

shouldReturn :: (MonadIO m, Show a, Eq a) => m a -> a -> m ()
shouldReturn m y = m >>= (`shouldBe` y)

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

-- | Check that the inner action does not modify the chain.

checkNoMod :: FlacMeta a -> FlacMeta a
checkNoMod m = do
  chainBefore <- getMetaChain
  result <- m
  chainAfter  <- getMetaChain
  chainAfter `shouldBe` chainBefore
  isMetaChainModified `shouldReturn` False
  return result

-- | MD5 sum of uncompressed audio data of the unmodified sample we use in
-- these tests.

refMD5Sum :: ByteString
refMD5Sum = B.pack [89,191,106,236,125,27,65,161,78,138,172,153,91,60,42,109]

-- | The sequence of metadata blocks as they appear in the unmodified sample
-- we use in these tests.

refChain :: NonEmpty MetadataType
refChain = StreamInfoBlock :| [VorbisCommentBlock, PaddingBlock]

-- | A dummy correct seek table.

testSeekTable :: Vector SeekPoint
testSeekTable = V.fromList
  [ SeekPoint 1 10 100
  , SeekPoint 2 20 108
  , SeekPoint 3 30 101 ]

-- | A dummy invalid seek table.

-- invalidSeekTable :: Vector SeekPoint
-- invalidSeekTable = V.fromList
--   [ SeekPoint 0 0 100
--   , SeekPoint 0 0 108
--   , SeekPoint 0 0 101 ]
