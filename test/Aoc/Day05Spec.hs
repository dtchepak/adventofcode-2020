module Aoc.Day05Spec (spec) where

import Aoc.Day05
import Test.Hspec

spec :: Spec
spec = do
  describe "seat id" $ do
    it "example" $ do
      seatId 44 5 `shouldBe` 357
      seatId 70 7 `shouldBe` 567
      seatId 14 7 `shouldBe` 119
      seatId 102 4 `shouldBe` 820

  describe "examples" $ do
    it "BFFFBBFRRR: row 70, column 7, seat ID 567" $ do
      parseRowAndCol "BFFFBBFRRR" `shouldBe` (70, 7)
    it "FFFBBBFRRR: row 14, column 7, seat ID 119" $ do
      parseRowAndCol "FFFBBBFRRR" `shouldBe` (14, 7)
    it "BBFFBBFRLL: row 102, column 4, seat ID 820" $ do
      parseRowAndCol "BBFFBBFRLL" `shouldBe` (102, 4)