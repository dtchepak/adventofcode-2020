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
        `shouldBe` Right (BagRule (Bag "light red") [(1, Bag "bright white"), (2, Bag "muted yellow")])
    it "example" $ do
      flip findBagsToContain (Bag "shiny gold") <$> readRules sampleRulesText
        `shouldBe` (Right . Set.fromList) [Bag "bright white", Bag "muted yellow", Bag "dark orange", Bag "light red"]
  describe "part 2" $ do
    it "lookup bag" $ do
      lookupBag (Bag "shiny gold") <$> readRules sampleRulesText
        `shouldBe` Right [(1, Bag "dark olive"), (2, Bag "vibrant plum")]
    it "example" $ do
      countBagsIn (Bag "shiny gold") <$> readRules sampleRulesText
        `shouldBe` Right 32
    it "example 2" $ do
      countBagsIn (Bag "shiny gold") <$> readRules sampleRules2
        `shouldBe` Right 126

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

sampleRules2 :: Text
sampleRules2 =
  T.unlines
    [ "shiny gold bags contain 2 dark red bags.",
      "dark red bags contain 2 dark orange bags.",
      "dark orange bags contain 2 dark yellow bags.",
      "dark yellow bags contain 2 dark green bags.",
      "dark green bags contain 2 dark blue bags.",
      "dark blue bags contain 2 dark violet bags.",
      "dark violet bags contain no other bags."
    ]