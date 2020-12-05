module Aoc.Day01Spec (spec) where

import Test.Hspec
import Aoc.Day01

spec :: Spec
spec = do
  describe "example data" $ do
    it "part 1: should find 1721 and 299" $ 
      findSum2020 [1721, 979, 366, 299, 675, 14561] `shouldBe` Just (1721, 299)
    it "part 2: should find  979, 366, and 675" $ 
      findTripleSum2020 [1721, 979, 366, 299, 675, 14561] `shouldBe` Just (979, 366, 675)