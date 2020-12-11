module Aoc.Day10Spec (spec) where

import Aoc.Day10
import qualified Data.Map as Map
import Test.Hspec

spec :: Spec
spec = do
  describe "example 1" $ do
    let input = [16, 10, 15, 5, 1, 11, 7, 19, 6, 12, 4]
    specify "built-in device" $ do
      builtIn input `shouldBe` 22
    specify "built-in handles empty" $ do
      builtIn [] `shouldBe` 0
    specify "jolt diffs" $ do
      joltDiffs input `shouldBe` Map.fromList [(1, 7), (3, 5)]
    specify "arrangements" $ do
      getArrangements input `shouldBe` Just 8
  describe "example 2" $ do
    let input = [28, 33, 18, 42, 31, 14, 46, 20, 48, 47, 24, 23, 49, 45, 19, 38, 39, 11, 1, 32, 25, 35, 8, 17, 7, 9, 4, 2, 34, 10, 3]
    specify "jolt diffs" $ do
      joltDiffs input `shouldBe` Map.fromList [(1, 22), (3, 10)]
    specify "arrangements" $ do
      getArrangements input `shouldBe` Just 19208
