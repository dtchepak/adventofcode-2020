
module Aoc.Day13Spec (spec) where

import Aoc.Day13
import Test.Hspec

spec :: Spec
spec = do
  describe "part 1" $ do
    specify "example" $ do
      earliest 939 [7, 13, 59, 31, 19] `shouldBe` Just 59