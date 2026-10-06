module LibSpec (spec) where

import Lib (parseDuration)
import Test.Hspec
  ( Spec,
    describe,
    it,
    shouldBe,
  )

spec :: Spec
spec = do
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

    it "parses empty string" $
      parseDuration "" `shouldBe` Right 0

    it "parses zero string" $
      parseDuration "0" `shouldBe` Right 0

    it "parses hours" $
      parseDuration "1h" `shouldBe` Right 3600

    it "parses fractional seconds" $
      parseDuration "1.5s" `shouldBe` Right 1.5

    it "parses milliseconds" $
      parseDuration "500ms" `shouldBe` Right 0.5
