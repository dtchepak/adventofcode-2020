module Aoc.Day04Spec (spec) where

import Aoc.Day04
import qualified Data.Text as T
import Test.Hspec

spec :: Spec
spec = do
  describe "sample data" $ do
    it "example has 2 valid passords" $ do
      isValidPassportsText (T.intercalate "\n\n" sampleData)
        `shouldBe` [True, False, True, False]

sampleData :: [T.Text]
sampleData =
  [ "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd\nbyr:1937 iyr:2017 cid:147 hgt:183cm",
    "iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884\nhcl:#cfa07d byr:1929",
    "hcl:#ae17e1 iyr:2013\neyr:2024\necl:brn pid:760753108 byr:1931\nhgt:179cm",
    "hcl:#cfa07d eyr:2025 pid:166559648\niyr:2011 ecl:brn hgt:59in"
  ]
