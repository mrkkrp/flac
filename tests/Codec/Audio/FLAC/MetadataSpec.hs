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

module Codec.Audio.FLAC.MetadataSpec
  ( spec )
where

import Codec.Audio.FLAC.Metadata
import Control.Monad.IO.Class (MonadIO (..))
import Crypto.Hash
import Data.Default.Class
import Test.Hspec

spec :: Spec
spec = -- dummy test for now
  describe "it" $
    it "works" $ do
      let path = "/home/mark/store/music/Adam Lambert/2015, The Original High/01 Ghost Town.flac"
      r <- runFlacMeta def path $ do
        retrieve MinBlockSize >>= liftIO . print
        retrieve MaxBlockSize >>= liftIO . print
        retrieve MinFrameSize >>= liftIO . print
        retrieve MaxFrameSize >>= liftIO . print
        retrieve SampleRate >>= liftIO . print
        retrieve Channels >>= liftIO . print
        retrieve BitsPerSample >>= liftIO . print
        retrieve TotalSamples >>= liftIO . print
        retrieve FileSize >>= liftIO . print
        retrieve BitRate  >>= liftIO . print
        digest <- digestFromByteString <$> retrieve MD5Sum
        liftIO . print $ (digest :: Maybe (Digest MD5))
        retrieve Duration >>= liftIO . print
        liftIO $ putStrLn "-----------------"
        retrieve VorbisVendor >>= liftIO . print
        retrieve (VorbisComment Title) >>= liftIO . print
        retrieve (VorbisComment Version) >>= liftIO . print
        retrieve (VorbisComment Album) >>= liftIO . print
        retrieve (VorbisComment TrackNumber) >>= liftIO . print
        retrieve (VorbisComment TrackTotal) >>= liftIO . print
        retrieve (VorbisComment Artist) >>= liftIO . print
        retrieve (VorbisComment Performer) >>= liftIO . print
        retrieve (VorbisComment Copyright) >>= liftIO . print
        retrieve (VorbisComment License) >>= liftIO . print
        retrieve (VorbisComment Organization) >>= liftIO . print
        retrieve (VorbisComment Description) >>= liftIO . print
        retrieve (VorbisComment Genre) >>= liftIO . print
        retrieve (VorbisComment Date) >>= liftIO . print
        retrieve (VorbisComment Location) >>= liftIO . print
        retrieve (VorbisComment Contact) >>= liftIO . print
        retrieve (VorbisComment ISRC) >>= liftIO . print
      r `shouldBe` Right ()
