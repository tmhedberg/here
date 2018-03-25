{-# LANGUAGE QuasiQuotes #-}

module Data.String.HereSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import qualified Data.Char as Char

import Data.String.Here

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "i quote" $ do
    it "should interpolate a number" $ do
      let val1   = 7878 :: Int
          expect = "value is 7878"
          actual = [i|value is ${val1}|]
      actual `shouldBe` expect

    -- (from: https://github.com/tmhedberg/here#readme)
    it "should interpolate a String value" $ do
      let foo = "foo"
          expect = "\"foo\", when capitalized, is FOO!"
          actual = [i|"foo", when capitalized, is ${map Char.toUpper foo}!|]
      actual `shouldBe` expect