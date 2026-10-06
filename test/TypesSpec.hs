module TypesSpec (spec) where

import Control.Monad (forM_)
import Test.Hspec
  ( Spec,
    describe,
    it,
    shouldBe,
  )
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Types

validSources :: [(String, Source)]
validSources =
  [ ("tcp://host:1234", Tcp "host" "1234"),
    ("tcp://127.0.0.1:80", Tcp "127.0.0.1" "80"),
    ("tcp://[::1]:80", Tcp "::1" "80"),
    ("tcp://[2001:db8::1]:1234", Tcp "2001:db8::1" "1234"),
    ("http://mock:8000/health", Http "http://mock:8000/health"),
    ("http://[::1]:8000/health", Http "http://[::1]:8000/health"),
    ("https://mock:8443/api/v1/health", Http "https://mock:8443/api/v1/health"),
    ("https://[2001:db8::1]:8443/api/v1/health", Http "https://[2001:db8::1]:8443/api/v1/health")
  ]

validChecks :: [(String, Maybe Check)]
validChecks =
  [ ("", Nothing),
    ("contains substr123", Just (Contains "substr123")),
    ("matches [0-9]+", Just (Matches "[0-9]+"))
  ]

genWhitespace :: Gen String
genWhitespace = do
  n <- choose (0, 8)
  vectorOf n (elements [' ', '\t'])

spec :: Spec
spec = do
  describe "regular parseProbe" $ do
    forM_ validSources $ \(srcStr, src) ->
      forM_ validChecks $ \(checkStr, check) -> do
        let middle = if null checkStr then "" else " " <> checkStr
            probeStr = srcStr <> middle
        it probeStr $ do
          parseProbe probeStr `shouldBe` Right (Probe src check)

  describe "whitespace tolerance (property)" $ do
    prop "arbitrary leading and trailing spaces do not affect the result" $
      forAll genWhitespace $ \leading ->
        forAll (elements validSources) $ \(srcStr, expectedSrc) ->
          forAll (elements validChecks) $ \(checkStr, expectedChk) ->
            let middle = if null checkStr then "" else " " <> checkStr
                probeStr = leading <> srcStr <> middle
             in parseProbe probeStr === Right (Probe expectedSrc expectedChk)

    prop "multiple spaces between source and check are tolerated" $
      forAll (choose (1, 5)) $ \spacesCount ->
        forAll (elements validSources) $ \(srcStr, expectedSrc) ->
          forAll (elements (filter (not . null . fst) validChecks)) $ \(checkStr, expectedChk) ->
            let spaces = replicate spacesCount ' '
                probeStr = srcStr <> spaces <> checkStr
             in parseProbe probeStr === Right (Probe expectedSrc expectedChk)

  describe "failed parseProbe" $ do
    it "rejects empty string" $ do
      parseProbe "" `shouldBe` Left "invalid probe "

    it "rejects whitespace only string" $ do
      parseProbe "   " `shouldBe` Left "invalid probe    "

    it "rejects invalid tcp port (non-digits)" $ do
      parseProbe "tcp://host:abc" `shouldBe` Left "invalid probe tcp://host:abc"

    it "rejects missing tcp port" $ do
      parseProbe "tcp://host:" `shouldBe` Left "invalid probe tcp://host:"
      parseProbe "tcp://host" `shouldBe` Left "invalid probe tcp://host"

    it "rejects unclosed ipv6 brackets" $ do
      parseProbe "tcp://[::1:80" `shouldBe` Left "invalid probe tcp://[::1:80"
      parseProbe "tcp://[:80" `shouldBe` Left "invalid probe tcp://[:80"

    it "rejects empty ipv6 brackets" $ do
      parseProbe "tcp://[]:80" `shouldBe` Left "invalid probe tcp://[]:80"

    it "rejects missing tcp port with ipv6" $ do
      parseProbe "tcp://[::1]:" `shouldBe` Left "invalid probe tcp://[::1]:"
      parseProbe "tcp://[::1]" `shouldBe` Left "invalid probe tcp://[::1]"

    it "rejects invalid tcp port with ipv6" $ do
      parseProbe "tcp://[::1]:abc" `shouldBe` Left "invalid probe tcp://[::1]:abc"

    it "rejects unsupported schemes" $ do
      parseProbe "ftp://files:21" `shouldBe` Left "invalid probe ftp://files:21"
      parseProbe "ws://stream:8080" `shouldBe` Left "invalid probe ws://stream:8080"

    it "rejects unknown check keyword" $ do
      parseProbe "tcp://host:1234 equals 200" `shouldBe` Left "invalid probe tcp://host:1234 equals 200"

    it "rejects check without argument" $ do
      parseProbe "tcp://host:1234 contains" `shouldBe` Left "invalid probe tcp://host:1234 contains"
      parseProbe "http://mock/health matches" `shouldBe` Left "invalid probe http://mock/health matches"

    it "doesn't reject multiple words in check argument" $ do
      parseProbe "tcp://host:1234 contains word1 word2" `shouldBe` Right (Probe (Tcp "host" "1234") (Just (Contains "word1 word2")))

    it "rejects invalid regex" $ do
      parseProbe "https://host:1234 matches [" `shouldBe` Left "invalid probe https://host:1234 matches ["

  describe "formatSource and formatProbe" $ do
    it "formats sources back into canonical strings" $ do
      forM_ validSources $ \(srcStr, src) ->
        formatSource src `shouldBe` srcStr

    it "formats probes with checks correctly" $ do
      formatProbe (Probe (Tcp "::1" "80") (Just (Contains "OK")))
        `shouldBe` "tcp://[::1]:80 contains OK"
      formatProbe (Probe (Tcp "host" "1234") Nothing)
        `shouldBe` "tcp://host:1234"
