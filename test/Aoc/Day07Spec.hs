module Aoc.Day07Spec (spec) where

import Aoc.Day07
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

spec :: Spec
spec = do
  describe "part 1" $ do
    it "parse a bag" $ do
      readBag "light red bags" `shouldBe` Right (Bag "light red")
    it "parse a singular bag" $ do
      readBag "dark orange bag" `shouldBe` Right (Bag "dark orange")
    it "parse a rule" $ do
      readRule (head sampleRules)
        `shouldBe` Right (BagRule (Bag "light red") [Bag "bright white", Bag "muted yellow"])
    it "example" $ do
      flip findBagsToContain (Bag "shiny gold") <$> readRules sampleRulesText
        `shouldBe` (Right . Set.fromList) [Bag "bright white", Bag "muted yellow", Bag "dark orange", Bag "light red"]

sampleRules :: [Text]
sampleRules =
  [ "light red bags contain 1 bright white bag, 2 muted yellow bags.",
    "dark orange bags contain 3 bright white bags, 4 muted yellow bags.",
    "bright white bags contain 1 shiny gold bag.",
    "muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.",
    "shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.",
    "dark olive bags contain 3 faded blue bags, 4 dotted black bags.",
    "vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.",
    "faded blue bags contain no other bags.",
    "dotted black bags contain no other bags."
  ]

sampleRulesText :: Text
sampleRulesText = T.unlines sampleRules