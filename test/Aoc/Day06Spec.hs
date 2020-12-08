module Aoc.Day06Spec (spec) where

import Aoc.Day06
import Test.Hspec

spec :: Spec
spec = do
  describe "part 1" $ do
    it "example" $ do
      part1 "abc\n\na\nb\nc\n\nab\nac\n\na\na\na\na\n\nb" `shouldBe` 11