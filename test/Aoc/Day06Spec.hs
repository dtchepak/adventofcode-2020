module Aoc.Day06Spec (spec) where

import Aoc.Day06
import Test.Hspec

spec :: Spec
spec = do
  let sample = "abc\n\na\nb\nc\n\nab\nac\n\na\na\na\na\n\nb"
  describe "part 1" $ do
    it "example" $ do
      part1 sample `shouldBe` 11
  describe "part 2" $ do
    it "example" $ do
      part2 sample `shouldBe` 6