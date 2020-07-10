{-# LANGUAGE OverloadedStrings #-}

module Codec.Audio.FLAC.MetadataSpec (spec) where

import Codec.Audio.FLAC.Metadata hiding (runFlacMeta)
import qualified Codec.Audio.FLAC.Metadata as Flac
import Codec.Audio.FLAC.Metadata.CueSheet
import Codec.Audio.Wave
import Control.Monad
import Control.Monad.IO.Class (MonadIO (..))
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Vector (Vector)
import qualified Data.Vector as V
import System.Directory
import System.IO
import System.IO.Temp (withSystemTempFile)
import Test.Hspec hiding (shouldBe, shouldReturn)
import qualified Test.Hspec as Hspec

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

  describe "Channels" $
    it "is read correctly" $ \path ->
      runFlacMeta def path . checkNoMod $
        retrieve Channels `shouldReturn` 2

  describe "ChannelMask" $
    it "is read correctly" $ \path ->
      runFlacMeta def path . checkNoMod $
        retrieve ChannelMask `shouldReturn` speakerStereo

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
        Application "foo" =-> Just "foo"
        Application "bobo" =-> Just "bobo"
        getMetaChain `shouldReturn` StreamInfoBlock
          :| [ApplicationBlock, ApplicationBlock, VorbisCommentBlock, PaddingBlock]
        isMetaChainModified `shouldReturn` True
      -- Can read it back.
      runFlacMeta def path . checkNoMod $ do
        retrieve (Application "foo") `shouldReturn` Just "foo"
        retrieve (Application "bobo") `shouldReturn` Just "bobo"
      -- Can wipe one without affecting the other.
      runFlacMeta def path $ do
        Application "foo" =-> Nothing
        retrieve (Application "foo") `shouldReturn` Nothing
        retrieve (Application "bobo") `shouldReturn` Just "bobo"
        getMetaChain `shouldReturn` StreamInfoBlock
          :| [ApplicationBlock, VorbisCommentBlock, PaddingBlock]
        isMetaChainModified `shouldReturn` True
      -- Can overwrite application data.
      runFlacMeta def path $ do
        Application "bobo" =-> Just "moon"
        retrieve (Application "bobo") `shouldReturn` Just "moon"
        getMetaChain `shouldReturn` StreamInfoBlock
          :| [ApplicationBlock, VorbisCommentBlock, PaddingBlock]
        isMetaChainModified `shouldReturn` True
      -- Can wipe the other one bringing it to the default state.
      runFlacMeta def path $ do
        Application "bobo" =-> Nothing
        getMetaChain `shouldReturn` refChain
        isMetaChainModified `shouldReturn` True
      runFlacMeta def path . checkNoMod $ do
        retrieve (Application "foo") `shouldReturn` Nothing
        retrieve (Application "bobo") `shouldReturn` Nothing

  describe "SeekTable" $ do
    it "raises exception when invalid seek table given" $ \path -> do
      let m =
            runFlacMeta def path $
              SeekTable =-> Just invalidSeekTable
      m `shouldThrow` (== MetaInvalidSeekTable)
    it "is set/read/deleted correctly" $ \path -> do
      -- Can set seek table if it's correct.
      runFlacMeta def path $ do
        SeekTable =-> Just testSeekTable
        getMetaChain `shouldReturn` StreamInfoBlock
          :| [SeekTableBlock, VorbisCommentBlock, PaddingBlock]
        isMetaChainModified `shouldReturn` True
      -- Can read it back.
      runFlacMeta def path . checkNoMod $
        retrieve SeekTable `shouldReturn` Just testSeekTable
      -- Can delete it.
      runFlacMeta def path $ do
        SeekTable =-> Nothing
        getMetaChain `shouldReturn` refChain
        isMetaChainModified `shouldReturn` True
      runFlacMeta def path . checkNoMod $
        SeekTable =-> Nothing
    context "when auto-vacuum disabled" $
      it "can write empty seek table" $ \path -> do
        runFlacMeta def {metaAutoVacuum = False} path $ do
          SeekTable =-> Just V.empty
          retrieve SeekTable `shouldReturn` Just V.empty
          getMetaChain `shouldReturn` StreamInfoBlock
            :| [SeekTableBlock, VorbisCommentBlock, PaddingBlock]
          isMetaChainModified `shouldReturn` True
        runFlacMeta def path . checkNoMod $
          retrieve SeekTable `shouldReturn` Just V.empty
    context "when auto-vacuum enabled" $
      it "empty seek table is removed automatically" $ \path -> do
        runFlacMeta def {metaAutoVacuum = True} path $ do
          SeekTable =-> Just V.empty
          retrieve SeekTable `shouldReturn` Just V.empty
          getMetaChain `shouldReturn` StreamInfoBlock
            :| [SeekTableBlock, VorbisCommentBlock, PaddingBlock]
          isMetaChainModified `shouldReturn` True
        runFlacMeta def path . checkNoMod $ do
          retrieve SeekTable `shouldReturn` Nothing
          getMetaChain `shouldReturn` refChain

  describe "VorbisVendor" $ do
    it "is set/read correctly" $ \path -> do
      -- Can set vorbis vendor.
      runFlacMeta def path $ do
        VorbisVendor =-> Just "foo"
        getMetaChain `shouldReturn` StreamInfoBlock
          :| [VorbisCommentBlock, PaddingBlock]
        isMetaChainModified `shouldReturn` True
      -- Can read it back.
      runFlacMeta def path . checkNoMod $
        retrieve VorbisVendor `shouldReturn` Just "foo"
    context "when auto-vacuum disabled" $
      it "deletion just sets the field to empty string" $ \path -> do
        runFlacMeta def {metaAutoVacuum = False} path $ do
          VorbisVendor =-> Nothing
          retrieve VorbisVendor `shouldReturn` Just ""
          getMetaChain `shouldReturn` StreamInfoBlock
            :| [VorbisCommentBlock, PaddingBlock]
          isMetaChainModified `shouldReturn` True
        runFlacMeta def path . checkNoMod $
          retrieve VorbisVendor `shouldReturn` Just ""
    context "when auto-vacuum enabled" $ do
      context "when no other vorbis fields set" $
        it "empty vendor causes removal of vorbis vendor block" $ \path -> do
          runFlacMeta def {metaAutoVacuum = True} path $ do
            VorbisVendor =-> Nothing
            retrieve VorbisVendor `shouldReturn` Just ""
            getMetaChain `shouldReturn` StreamInfoBlock
              :| [VorbisCommentBlock, PaddingBlock]
            isMetaChainModified `shouldReturn` True
          runFlacMeta def path . checkNoMod $ do
            retrieve VorbisVendor `shouldReturn` Nothing
            getMetaChain `shouldReturn` StreamInfoBlock :| [PaddingBlock]
      context "when other vorbis fields exist" $
        it "deletion just sets the field to empty string" $ \path -> do
          runFlacMeta def {metaAutoVacuum = True} path $ do
            VorbisComment Title =-> Just "bobla"
            VorbisVendor =-> Nothing
            retrieve VorbisVendor `shouldReturn` Just ""
            getMetaChain `shouldReturn` StreamInfoBlock
              :| [VorbisCommentBlock, PaddingBlock]
            isMetaChainModified `shouldReturn` True
          runFlacMeta def path . checkNoMod $ do
            retrieve VorbisVendor `shouldReturn` Just ""
            getMetaChain `shouldReturn` StreamInfoBlock
              :| [VorbisCommentBlock, PaddingBlock]

  describe "VorbisComment" . forM_ [minBound .. maxBound] $ \vfield ->
    it (show vfield ++ " is set/read/deleted correctly") $ \path -> do
      -- Can set vorbis comment.
      runFlacMeta def path $ do
        VorbisComment vfield =-> Just "foo"
        getMetaChain `shouldReturn` StreamInfoBlock
          :| [VorbisCommentBlock, PaddingBlock]
        isMetaChainModified `shouldReturn` True
      -- Can read it back.
      runFlacMeta def path . checkNoMod $
        retrieve (VorbisComment vfield) `shouldReturn` Just "foo"
      -- Can delete it.
      runFlacMeta def path $ do
        VorbisComment vfield =-> Nothing
        retrieve (VorbisComment vfield) `shouldReturn` Nothing
        getMetaChain `shouldReturn` refChain
        isMetaChainModified `shouldReturn` True
      runFlacMeta def path . checkNoMod $
        retrieve (VorbisComment vfield) `shouldReturn` Nothing

  describe "CueSheet" $ do
    context "when the CUE sheet is for a CD" $
      it "raises exception when invalid CDDA CUE sheet is given" $ \path -> do
        let m =
              runFlacMeta def path $
                CueSheet =-> Just invalidCueSheet {cueIsCd = True}
            leadInError = "CD-DA cue sheet must have a lead-in length of at least 2 seconds"
        m `shouldThrow` (== MetaInvalidCueSheet leadInError)
    context "when the CUE sheet is not for a CD" $
      it "does not find anything bad in given CUE sheet" $ \path ->
        -- NOTE All other possible issues have been taken care of by the
        -- type system and carefully arranged data type definitions.
        runFlacMeta def path $
          CueSheet =-> Just invalidCueSheet {cueIsCd = False}
    it "is set/read/deleted correctly" $ \path -> do
      -- Can set CUE sheet if it's correct.
      runFlacMeta def path $ do
        CueSheet =-> Just testCueSheet
        getMetaChain `shouldReturn` StreamInfoBlock
          :| [CueSheetBlock, VorbisCommentBlock, PaddingBlock]
        isMetaChainModified `shouldReturn` True
      -- Can read it back.
      runFlacMeta def path . checkNoMod $
        retrieve CueSheet `shouldReturn` Just testCueSheet
      -- Can delete it.
      runFlacMeta def path $ do
        CueSheet =-> Nothing
        getMetaChain `shouldReturn` refChain
        isMetaChainModified `shouldReturn` True
      runFlacMeta def path . checkNoMod $
        CueSheet =-> Nothing

  describe "Picture" . forM_ [minBound .. maxBound] $ \ptype -> do
    it (show ptype ++ " raises exception on invalid picture") $ \path -> do
      let m =
            runFlacMeta def path $
              Picture ptype =-> Just invalidPicture
          mimeTypeError = "MIME type string must contain only printable ASCII characters (0x20-0x7e)"
      m `shouldThrow` (== MetaInvalidPicture mimeTypeError)
    it (show ptype ++ " is set/read/deleted correctly") $ \path -> do
      -- Can set a picture.
      runFlacMeta def path $ do
        Picture ptype =-> Just testPicture
        getMetaChain `shouldReturn` StreamInfoBlock
          :| [PictureBlock, VorbisCommentBlock, PaddingBlock]
        isMetaChainModified `shouldReturn` True
      -- Can read it back.
      runFlacMeta def path . checkNoMod $
        retrieve (Picture ptype) `shouldReturn` Just testPicture
      -- Can delete it.
      runFlacMeta def path $ do
        Picture ptype =-> Nothing
        getMetaChain `shouldReturn` refChain
        isMetaChainModified `shouldReturn` True
      runFlacMeta def path . checkNoMod $
        Picture ptype =-> Nothing

  describe "wipeVorbisComment" $
    it "wipes all “vorbis comment” metadata blocks" $ \path -> do
      runFlacMeta def path $ do
        VorbisComment Title =-> Just "Title"
        VorbisComment Artist =-> Just "Artist"
      runFlacMeta def path $ do
        wipeVorbisComment
        getMetaChain `shouldReturn` StreamInfoBlock :| [PaddingBlock]
        isMetaChainModified `shouldReturn` True
      runFlacMeta def path . checkNoMod $
        getMetaChain `shouldReturn` StreamInfoBlock :| [PaddingBlock]

  describe "wipeApplications" $
    it "wipes all “application” metadata blocks" $ \path -> do
      runFlacMeta def path $ do
        Application "foo" =-> Just "foo"
        Application "bobo" =-> Just "bobo"
      runFlacMeta def path $ do
        wipeApplications
        getMetaChain `shouldReturn` refChain
        isMetaChainModified `shouldReturn` True
      runFlacMeta def path . checkNoMod $
        getMetaChain `shouldReturn` refChain

  describe "wipeSeekTable" $
    it "wipes all “seek table” metadata blocks" $ \path -> do
      runFlacMeta def path $
        SeekTable =-> Just testSeekTable
      runFlacMeta def path $ do
        wipeSeekTable
        getMetaChain `shouldReturn` refChain
        isMetaChainModified `shouldReturn` True
      runFlacMeta def path . checkNoMod $
        getMetaChain `shouldReturn` refChain

  describe "wipeCueSheets" $
    it "wipes all “CUE sheet” metadata blocks" $ \path -> do
      runFlacMeta def path $
        CueSheet =-> Just testCueSheet
      runFlacMeta def path $ do
        wipeCueSheets
        getMetaChain `shouldReturn` refChain
        isMetaChainModified `shouldReturn` True
      runFlacMeta def path . checkNoMod $
        getMetaChain `shouldReturn` refChain

  describe "wipePictures" $
    it "wipes all “picture” metadata blocks" $ \path -> do
      runFlacMeta def path $ do
        Picture PictureFrontCover =-> Just testPicture
        Picture PictureBackCover =-> Just testPicture
      runFlacMeta def path $ do
        wipePictures
        getMetaChain `shouldReturn` refChain
        isMetaChainModified `shouldReturn` True
      runFlacMeta def path . checkNoMod $
        getMetaChain `shouldReturn` refChain

----------------------------------------------------------------------------
-- Helpers

-- | A shortcut for 'defaultMetaSettings'.
def :: MetaSettings
def = defaultMetaSettings

infix 1 `shouldBe`, `shouldReturn`

-- | Lifted 'Hspec.shouldBe'.
shouldBe :: (MonadIO m, Show a, Eq a) => a -> a -> m ()
shouldBe x y = liftIO (x `Hspec.shouldBe` y)

-- | Lifted 'Hspec.shouldReturn'.
shouldReturn :: (MonadIO m, Show a, Eq a) => m a -> a -> m ()
shouldReturn m y = m >>= (`shouldBe` y)

-- | Type constrained version of 'Flac.runFlacMeta' to remove type
-- ambiguity.
runFlacMeta :: MetaSettings -> FilePath -> FlacMeta a -> IO a
runFlacMeta = Flac.runFlacMeta

-- | Make a temporary copy of @audio-samples/sample.flac@ file and provide
-- the path to the file. Automatically remove the file when the test
-- finishes.
withSandbox :: ActionWith FilePath -> IO ()
withSandbox action = withSystemTempFile "sample.flac" $ \path h -> do
  hClose h
  copyFile "audio-samples/sample.flac" path
  action path

-- | Check that the inner action does not modify the chain.
checkNoMod :: FlacMeta a -> FlacMeta a
checkNoMod m = do
  chainBefore <- getMetaChain
  result <- m
  chainAfter <- getMetaChain
  chainAfter `shouldBe` chainBefore
  isMetaChainModified `shouldReturn` False
  return result

-- | MD5 sum of uncompressed audio data of the unmodified sample we use in
-- these tests.
refMD5Sum :: ByteString
refMD5Sum = B.pack [89, 191, 106, 236, 125, 27, 65, 161, 78, 138, 172, 153, 91, 60, 42, 109]

-- | The sequence of metadata blocks as they appear in the unmodified sample
-- we use in these tests.
refChain :: NonEmpty MetadataType
refChain = StreamInfoBlock :| [VorbisCommentBlock, PaddingBlock]

-- | A correct seek table.
testSeekTable :: Vector SeekPoint
testSeekTable =
  V.fromList
    [ SeekPoint 1 10 100,
      SeekPoint 2 20 108,
      SeekPoint 3 30 101
    ]

-- | An invalid seek table.
invalidSeekTable :: Vector SeekPoint
invalidSeekTable =
  V.fromList
    [ SeekPoint 0 0 100,
      SeekPoint 0 0 108,
      SeekPoint 0 0 101
    ]

-- | A correct CUE sheet.
testCueSheet :: CueSheetData
testCueSheet =
  CueSheetData
    { cueCatalog = "1112223334445",
      cueLeadIn = 88200, -- at least two seconds
      cueIsCd = True,
      cueTracks =
        [ CueTrack
            { cueTrackOffset = 588 * 2,
              cueTrackIsrc = "abcde1234567",
              cueTrackAudio = True,
              cueTrackPreEmphasis = True,
              cueTrackPregapIndex = Nothing,
              cueTrackIndices = NE.fromList [0, 588, 588 * 2]
            },
          CueTrack
            { cueTrackOffset = 588 * 3,
              cueTrackIsrc = "abced1234576",
              cueTrackAudio = False,
              cueTrackPreEmphasis = False,
              cueTrackPregapIndex = Just 588,
              cueTrackIndices = NE.fromList [0, 588, 588 * 7]
            }
        ],
      cueLeadOutTrack =
        CueTrack
          { cueTrackOffset = 588 * 10,
            cueTrackIsrc = "",
            cueTrackAudio = True,
            cueTrackPreEmphasis = False,
            cueTrackPregapIndex = Just (588 * 9),
            cueTrackIndices = NE.fromList [0]
          }
    }

-- | An invalid CUE sheet.
invalidCueSheet :: CueSheetData
invalidCueSheet =
  CueSheetData
    { cueCatalog = "1112223334445",
      cueLeadIn = 1401, -- less than two seconds — illegal
      cueIsCd = True,
      cueTracks =
        [ CueTrack
            { cueTrackOffset = 1212,
              cueTrackIsrc = "abcde1234567",
              cueTrackAudio = True,
              cueTrackPreEmphasis = True,
              cueTrackPregapIndex = Nothing,
              cueTrackIndices = NE.fromList [0, 13, 1096]
            },
          CueTrack
            { cueTrackOffset = 1313,
              cueTrackIsrc = "abced1234576",
              cueTrackAudio = False,
              cueTrackPreEmphasis = False,
              cueTrackPregapIndex = Just 588,
              cueTrackIndices = NE.fromList [0, 19, 1069]
            }
        ],
      cueLeadOutTrack =
        CueTrack
          { cueTrackOffset = 8888,
            cueTrackIsrc = "",
            cueTrackAudio = True,
            cueTrackPreEmphasis = False,
            cueTrackPregapIndex = Just 588,
            cueTrackIndices = NE.fromList [0]
          }
    }

-- | A correct picture.
testPicture :: PictureData
testPicture =
  PictureData
    { pictureMimeType = "application/jpeg",
      pictureDescription = "Good description.",
      pictureWidth = 100,
      pictureHeight = 100,
      pictureDepth = 24,
      pictureColors = 0,
      pictureData = "Some picture data goes here, honest."
    }

-- | An invalid picture.
invalidPicture :: PictureData
invalidPicture =
  PictureData
    { pictureMimeType = "application\1/jpeg",
      pictureDescription = "Bad\1 description.",
      pictureWidth = 100,
      pictureHeight = 100,
      pictureDepth = 24,
      pictureColors = 0,
      pictureData = "Some picture data goes here, honest."
    }
