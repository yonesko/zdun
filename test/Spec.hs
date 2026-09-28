module Main (main) where

import Lib (parseDuration)
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "parseDuration" $ do
    it "parses single digit seconds" $
      parseDuration "1s" `shouldBe` Right 1

    it "parses multi digit seconds" $
      parseDuration "12s" `shouldBe` Right 12

    it "parses single digit minutes" $
      parseDuration "2m" `shouldBe` Right 120

    it "parses multi digit minutes" $
      parseDuration "10m" `shouldBe` Right 600

    it "parses multi digit minutes with multi digit seconds" $
      parseDuration "10m77s" `shouldBe` Right 677

    it "rejects invalid characters" $
      parseDuration "1x" `shouldBe` Left "Invalid duration 1x"

    it "rejects strings starting with non-digit" $
      parseDuration "s1" `shouldBe` Left "Invalid duration s1"

    it "rejects empty string" $
      parseDuration "" `shouldBe` Right 0
