module Aoc.Day02Spec (spec) where

import Test.Hspec
import Aoc.Day02

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
      part1 sampleData `shouldBe` pure 2
  describe "part 2" $ do
    it "1-3 a: abcde is valid: position 1 contains a and position 3 does not." $
      PasswordPolicy 1 3 'a' `isValidPasswordRevised` "abcde"
    it "1-3 b: cdefg is invalid: neither position 1 nor position 3 contains b." $
      not . isValidPasswordRevised (PasswordPolicy 1 3 'b') $ "cdefg"
    it "2-9 c: ccccccccc is invalid: both position 2 and position 9 contain c." $
      not . isValidPasswordRevised (PasswordPolicy 2 9 'c') $ "ccccccccc"