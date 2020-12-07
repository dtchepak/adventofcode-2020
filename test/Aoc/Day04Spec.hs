module Aoc.Day04Spec (spec) where

import Aoc.Day04
import Data.Either (isLeft)
import qualified Data.Text as T
import Test.Hspec

spec :: Spec
spec = do
  let allPassports = T.intercalate "\n\n"

  describe "part 1 sample data" $ do
    it "example has 2 valid passports" $ do
      isValidPassportsText (allPassports sampleData)
        `shouldBe` [True, False, True, False]

  describe "part 2 parsing" $ do
    it "birth years: valid" $ do
      parseAll parseBirthYear <$> ["1920", "2002", "2000"]
        `shouldBe` [pure 1920, pure 2002, pure 2000]
    it "birth years: invalid" $ do
      isLeft . parseAll parseBirthYear <$> ["2003", "1919", "123", "asd", "20000"]
        `shouldBe` [True, True, True, True, True]
    it "heights: valid" $ do
      parseAll parseHeight <$> ["150cm", "59in", "193cm", "76in"]
        `shouldBe` (pure <$> [Cm 150, Inches 59, Cm 193, Inches 76])
    it "heights: invalid" $ do
      isLeft . parseAll parseHeight <$> ["150bits", "149cm", "77in", "77inches"]
        `shouldBe` [True, True, True, True]
    it "hair colour: valid" $ do
      parseAll parseHairColour <$> ["#1234af", "#000000"]
        `shouldBe` (pure <$> ["1234af", "000000"])
    it "hair colour: invalid" $ do
      isLeft . parseAll parseHairColour <$> ["123456", "#zzzzzz", "#000"]
        `shouldBe` [True, True, True]
    it "eye colour: valid" $ do
      parseAll parseEyeColour <$> ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
        `shouldBe` (pure <$> ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"])
    it "eye colour: invalid" $ do
      isLeft . parseAll parseEyeColour <$> ["am", "bl", "br", "gr", "gr", "hz", "other"]
        `shouldBe` [True, True, True, True, True, True, True]
    it "id: valid" $ do
      parseAll parseId <$> ["123456789", "000000000"]
        `shouldBe` (pure <$> ["123456789", "000000000"])
    it "id: invalid" $ do
      isLeft . parseAll parseId <$> ["1234567890", "0a0000000", "1234"]
        `shouldBe` [True, True, True]

  describe "part 2 examples" $ do
    it "first valid example" $ do
      validatePassportValues (parsePassport (head part2ValidExamples)) `shouldBe` Right ()
    it "invalid passport examples find 0 valid items" $ do
      part2 (allPassports part2InvalidExamples) `shouldBe` 0
    it "finds 4 examples of valid passports" $ do
      part2 (allPassports part2ValidExamples) `shouldBe` 4

sampleData :: [T.Text]
sampleData =
  [ "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd\nbyr:1937 iyr:2017 cid:147 hgt:183cm",
    "iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884\nhcl:#cfa07d byr:1929",
    "hcl:#ae17e1 iyr:2013\neyr:2024\necl:brn pid:760753108 byr:1931\nhgt:179cm",
    "hcl:#cfa07d eyr:2025 pid:166559648\niyr:2011 ecl:brn hgt:59in"
  ]

part2InvalidExamples :: [T.Text]
part2InvalidExamples =
  [ "eyr:1972 cid:100\nhcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926",
    "iyr:2019\nhcl:#602927 eyr:1967 hgt:170cm\necl:grn pid:012533040 byr:1946",
    "hcl:dab227 iyr:2012\necl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277",
    "hgt:59cm ecl:zzz\neyr:2038 hcl:74454a iyr:2023\npid:3556412378 byr:2007"
  ]

part2ValidExamples :: [T.Text]
part2ValidExamples =
  [ "pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980\nhcl:#623a2f",
    "eyr:2029 ecl:blu cid:129 byr:1989\niyr:2014 pid:896056539 hcl:#a97842 hgt:165cm",
    "hcl:#888785\nhgt:164cm byr:2001 iyr:2015 cid:88\npid:545766238 ecl:hzl\neyr:2022",
    "iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719"
  ]