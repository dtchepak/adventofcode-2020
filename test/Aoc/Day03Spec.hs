
module Aoc.Day03Spec (spec) where

import Test.Hspec
import Aoc.Day03
import Data.Text
import qualified Data.List.NonEmpty as NEL

spec :: Spec
spec = do
  describe "sample tree locations" $ do
    let trees = parseTreeLocations sampleInput
    let pt = Point2D
    it "does not have a tree at (0, 0)" $ do
        trees `hasNoTree` pt (0, 0)
    it "has a tree at (3, 0)" $ do
        trees `hasTree` pt (3, 0)
    it "has a tree at (6, 2)" $ do
        trees `hasTree` pt (6, 2)
    it "does not have a tree at (1, 1)" $ do
        trees `hasNoTree` pt (1, 1)
    it "has a tree at (0, 1)" $ do
        trees `hasTree` pt (0, 1)
    it "wraps, does not have a tree at (10, 0)" $ do
        trees `hasNoTree` pt (10, 0)
    it "wraps, has a tree at (13, 0)" $ do
        trees `hasTree` pt (13, 0)
    it "has no trees after bottom of input" $ do
        trees `hasNoTree` pt (0, 1000)
    it "height of sample data" $ do
        height trees `shouldBe` 11
    it "width of sample data" $ do
        width trees `shouldBe` 11
    it "trees in 1st row" $ do
        hasTree trees . pt . (,0) <$> [0..10]
            `shouldBe` [ False, False, True, True, False, False, False, False, False, False, False ]
    it "trees in 2nd row" $ do
        hasTree trees . pt . (,1) <$> [0..10]
            `shouldBe` [ True, False, False, False, True, False, False, False, True, False, False ]
    it "adding pts" $ do
        pt (1,2) <> pt (10, 20) `shouldBe` pt (11, 22)
    it "locations on slope (3, 1)" $ do
        NEL.take 5 (gradient (pt (3, 1)) mempty) `shouldBe`
            (pt <$> [ (0, 0), (3, 1), (6, 2), (9, 3), (12, 4)])
    it "has 7 trees on slope (3, 1)" $ do
        treeCountOnSlope trees (pt (3, 1)) `shouldBe` 7

sampleInput :: Text
sampleInput = intercalate "\n"
    [ "..##......."
    , "#...#...#.."
    , ".#....#..#."
    , "..#.#...#.#"
    , ".#...##..#."
    , "..#.##....."
    , ".#.#.#....#"
    , ".#........#"
    , "#.##...#..."
    , "#...##....#"
    , ".#..#...#.#"
    ]