module Aoc.Day05Spec (spec) where

import Aoc.Day05
import Test.Hspec

spec :: Spec
spec = do
  describe "seat id" $ do
    it "example" $ do
      seatId 44 5 `shouldBe` 357

  describe "row search" $ do
    let rowTree = binarySpace 0 127

    it "upper and lower partition examples" $ do
      lower (Range 0 127) `shouldBe` Range 0 63
      upper (Range 0 63) `shouldBe` Range 32 63
      lower (Range 32 63) `shouldBe` Range 32 47
      upper (Range 32 47) `shouldBe` Range 40 47
      upper (Range 40 47) `shouldBe` Range 44 47
      lower (Range 44 47) `shouldBe` Range 44 45
      lower (Range 44 45) `shouldBe` Value 44

    it "search space FBFBBFF" $ do
      let path = [LeftPath, RightPath, LeftPath, RightPath, RightPath, LeftPath, LeftPath]
      followPath path rowTree `shouldBe` Leaf (Value 44)

    it "search space BFFFBBF" $ do
      let path = [RightPath, LeftPath, LeftPath, LeftPath, RightPath, RightPath, LeftPath]
      followPath path rowTree `shouldBe` Leaf (Value 70)

  describe "column search" $ do
    let cols = binarySpace 0 7
    it "example RLR" $ do
      let path = [RightPath, LeftPath, RightPath]
      followPath path cols `shouldBe` Leaf (Value 5)
