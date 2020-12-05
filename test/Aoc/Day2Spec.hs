module Aoc.Day2Spec (spec) where

import Test.Hspec
import Aoc.Day2

spec :: Spec
spec = do
  describe "password policy" $ do
    let twoToFiveZPolicy = PasswordPolicy 2 5 'z'
    it "invalid password (min req not met)" $
      not . isValidPassword twoToFiveZPolicy $ "abczde"
    it "invalid password (max req not met)" $
      not . isValidPassword twoToFiveZPolicy $ "abzczzdezzzz"
    it "valid password" $
      isValidPassword twoToFiveZPolicy "abzczde"
  describe "parsing" $ do
    it "matches happy case" $
      parsePasswordListLine "1-2 a: bcd" `shouldBe` pure (PasswordPolicy 1 2 'a', "bcd")
    it "fails on invalid min int" $
      parsePasswordListLine "z-2 a: bcd" `shouldBe` Left (InvalidInt "z")
    it "fails on invalid max int" $
      parsePasswordListLine "1-z a: bcd" `shouldBe` Left (InvalidInt "z")
    it "fails on invalid char" $
      parsePasswordListLine "1-2 azz: bcd" `shouldBe` Left (InvalidChar "azz")
    it "fails if no space after policy" $
      parsePasswordListLine "1-2 a:b bcd" `shouldBe` Left (ExpectedEmpty "b")
    it "fails if too many elements given" $
      parsePasswordListLine "1-2 a: bcd blah blah"
        `shouldBe` Left (UnrecognisedInput ["1", "2", "a", "", "bcd", "blah", "blah"])
    it "fails if insufficient elements given" $
      parsePasswordListLine "1-2 a" `shouldBe` Left (UnrecognisedInput ["1", "2", "a"])
  describe "part 1" $ do
    let sampleData = "1-3 a: abcde\n1-3 b: cdefg\n2-9 c: ccccccccc"
    it "part 1: parse sample data" $ 
      parsePasswordList sampleData
        `shouldBe` pure [
          (PasswordPolicy 1 3 'a', "abcde")
          , (PasswordPolicy 1 3 'b', "cdefg")
          , (PasswordPolicy 2 9 'c', "ccccccccc")
        ]
    it "part 1: sample data has 2 valid passwords" $ 
      validPasswords sampleData `shouldBe` pure 2
