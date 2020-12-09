module Aoc.Day09Spec (spec) where

import Aoc.Day09
import qualified Data.Vector.Unboxed as V
import Test.Hspec

spec :: Spec
spec = do
  let input = V.fromList [35, 20, 15, 25, 47, 40, 62, 55, 65, 95, 102, 117, 150, 182, 127, 219, 299, 277, 309, 576]
  describe "part 1" $ do
    it "intro example" $ do
      findFirstInvalid 5 input `shouldBe` Invalid 127

  describe "part 2" $ do
    it "contiguous sum" $ do
      contiguousSum 4 (V.fromList [1 .. 10]) `shouldBe` (1 + 2 + 3 + 4)
    it "search slice" $ do
      searchSlice 127 (V.drop 2 input) `shouldBe` (pure . V.fromList) [15, 25, 47, 40]
    it "contiguous sum example" $ do
      findContiguousSum 127 input `shouldBe` (pure . V.fromList) [15, 25, 47, 40]