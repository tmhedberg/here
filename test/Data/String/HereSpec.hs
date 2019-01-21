{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.String.HereSpec where

import Test.Hspec

import qualified Data.Char as Char
import qualified Data.ByteString as BS
import qualified Data.Text as T

import Data.String.Here

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "i quote" $ do
    it "should interpolate a number" $ do
      let val1 :: Int
          val1   = 7878
          expect :: String
          expect = "value is 7878"
          actual :: String
          actual = [i|value is ${val1}|]
      actual `shouldBe` expect

    -- (from: https://github.com/tmhedberg/here#readme)
    it "should interpolate a String expression" $ do
      let foo :: String
          foo = "foo"
          expect :: String
          expect = "\"foo\", when capitalized, is FOO!"
          actual :: String
          actual = [i|"foo", when capitalized, is ${map Char.toUpper foo}!|]
      actual `shouldBe` expect

    it "should interpolate a number to a ByteString" $ do
      let val1 :: Int
          val1   = 3535
          expect :: BS.ByteString
          expect = "value is 3535"
          actual :: BS.ByteString
          actual = [i|value is ${val1}|]
      actual `shouldBe` expect

    it "should interpolate a number to a Text" $ do
      let val1 :: Int
          val1   = 9988
          expect :: T.Text
          expect = "value is 9988"
          actual :: T.Text
          actual = [i|value is ${val1}|]
      actual `shouldBe` expect

  describe "iTrimMargins quote" $ do
    it "should trim leading whiltespaces" $ do
      let expected :: String
          expected = "Hi\n:D"
          actual :: String
          actual = [iTrimMargins|
            Hi
            :D
          |]
      actual `shouldBe` expected

  -- (from: https://github.com/tmhedberg/here#readme)
  describe "here quote" $ do
    it "should be here docs" $ do
      let expect :: String
          expect = "Hello world,\n\nI am a multiline here doc!"
          actual :: String
          actual = [here|
Hello world,

I am a multiline here doc!
|]
      actual `shouldBe` expect
