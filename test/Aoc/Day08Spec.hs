module Aoc.Day08Spec (spec) where

import Aoc.Day08
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

spec :: Spec
spec = do
  describe "part 1 example" $ do
    it "acc on dupe instruction" $ do
      part1 sampleProgram `shouldBe` Right (1, 5)
  describe "part 2 exampel" $ do
    it "finds uncorrupt program" $ do
      part2 sampleProgram `shouldBe` Just (9, 8)

sampleProgram :: Text
sampleProgram =
  T.unlines
    [ "nop +0",
      "acc +1",
      "jmp +4",
      "acc +3",
      "jmp -3",
      "acc -99",
      "acc +1",
      "jmp -4",
      "acc +6"
    ]