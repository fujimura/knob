{-# LANGUAGE OverloadedStrings #-}

module KnobSpec ( main, spec ) where


import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString
import           Data.ByteString.Char8  ()
import           Data.ByteString.Unsafe (unsafePackCStringLen)
import           Foreign                (nullPtr)
import qualified GHC.IO.Exception       as GHC
import           System.IO
import           Test.Hspec

import           Data.Knob

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  it "should read from start" $ do
    k <- newKnob "abcde"
    h <- newFileHandle k "foo.txt" ReadMode

    bytes <- Data.ByteString.hGet h 3
    bytes `shouldBe` "abc"

    off <- hTell h
    off `shouldBe` 3

  it "should read from offset" $ do
    k <- newKnob "abcde"
    h <- newFileHandle k "foo.txt" ReadMode

    hSeek h AbsoluteSeek 1
    bytes <- Data.ByteString.hGet h 3
    bytes `shouldBe` "bcd"

    off <- hTell h
    off `shouldBe` 4

  it "should read to EOF" $ do
    k <- newKnob "abcde"
    h <- newFileHandle k "foo.txt" ReadMode

    bytes <- Data.ByteString.hGet h 10
    bytes `shouldBe` "abcde"

    off <- hTell h
    off `shouldBe` 5

  it "should read past EOF" $ do
    k <- newKnob "abcde"
    h <- newFileHandle k "foo.txt" ReadMode

    hSeek h AbsoluteSeek 10
    bytes <- Data.ByteString.hGet h 10
    bytes `shouldBe` ""

    off <- hTell h
    off `shouldBe` 10
