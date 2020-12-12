module Aoc.Day11Spec (spec) where

import Aoc.Array2D
import Aoc.Day11
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Test.Hspec

pt :: (Int, Int) -> Point2D
pt = Point2D

spec :: Spec
spec = do
  describe "part 1 examples" $ do
    specify "gen one" $ do
      gen (importArray gen0Example) `shouldBe` importArray gen1Example
    specify "gen two" $ do
      gen (gen (importArray gen0Example)) `shouldBe` importArray gen2Example
    specify "stable gen" $ do
      run (importArray gen0Example) `shouldBe` importArray genStableExample
    specify "stable gen filled" $ do
      countOccupied (run (importArray gen0Example)) `shouldBe` 37
    specify "adjacent seats" $ do
      adjacentSeats (importArray gen1Example) (pt (0, 0)) `shouldBe` ".##"
    describe "neighours" $ do
      let check = lookupPt (importArray gen1Example) . pt
      specify "nw" $ do
        check (-1, -1) `shouldBe` Nothing
      specify "n" $ do
        check (0, -1) `shouldBe` Nothing
      specify "ne" $ do
        check (1, -1) `shouldBe` Nothing
      specify "w" $ do
        check (-1, 0) `shouldBe` Nothing
      specify "e" $ do
        check (1, 0) `shouldBe` Just '.'
      specify "sw" $ do
        check (-1, 1) `shouldBe` Nothing
      specify "s" $ do
        check (0, 1) `shouldBe` Just '#'
      specify "se" $ do
        check (1, 1) `shouldBe` Just '#'
    specify "adjacent seats debug" $ do
      let values = lookupPts (importArray gen1Example) . neighbours $ pt (0, 0)
      values `shouldBe` [Nothing, Nothing, Nothing, Nothing, Just '.', Nothing, Just '#', Just '#']
    specify "test update gen1" $ do
      updatePosition (importArray gen1Example) (pt (0, 0)) '#' `shouldBe` '#'

showArray :: Array2D Char -> String
showArray (Array2D (Width w) v) =
  let rows [] = []
      rows xs@(_ : _) = take w xs : rows (drop w xs)
   in unlines (rows (V.toList v))

gen0Example :: Text
gen0Example =
  T.unlines
    [ "L.LL.LL.LL",
      "LLLLLLL.LL",
      "L.L.L..L..",
      "LLLL.LL.LL",
      "L.LL.LL.LL",
      "L.LLLLL.LL",
      "..L.L.....",
      "LLLLLLLLLL",
      "L.LLLLLL.L",
      "L.LLLLL.LL"
    ]

gen1Example :: Text
gen1Example =
  T.unlines
    [ "#.##.##.##",
      "#######.##",
      "#.#.#..#..",
      "####.##.##",
      "#.##.##.##",
      "#.#####.##",
      "..#.#.....",
      "##########",
      "#.######.#",
      "#.#####.##"
    ]

gen2Example :: Text
gen2Example =
  T.unlines
    [ "#.LL.L#.##",
      "#LLLLLL.L#",
      "L.L.L..L..",
      "#LLL.LL.L#",
      "#.LL.LL.LL",
      "#.LLLL#.##",
      "..L.L.....",
      "#LLLLLLLL#",
      "#.LLLLLL.L",
      "#.#LLLL.##"
    ]

genStableExample :: Text
genStableExample =
  T.unlines
    [ "#.#L.L#.##",
      "#LLL#LL.L#",
      "L.#.L..#..",
      "#L##.##.L#",
      "#.#L.LL.LL",
      "#.#L#L#.##",
      "..L.L.....",
      "#L#L##L#L#",
      "#.LLLLLL.L",
      "#.#L#L#.##"
    ]