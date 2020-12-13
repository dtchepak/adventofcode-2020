module Aoc.Day11Spec (spec) where

import Aoc.Array2D
import Aoc.Day11
import Data.Text (Text)
import qualified Data.Text as T
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

  describe "part 2 examples" $ do
    let exampleWith8 = importArray part2ExampleWith8
    specify "pts in direction" $ do
      ptsInDirection exampleWith8 (pt (1, 1)) (pt (3, 4))
        `shouldBe` pt <$> [(4, 5), (5, 6), (6, 7), (7, 8)]
    specify "look directions" $ do
      flip (firstSeatInDirection exampleWith8) (pt (3, 4)) <$> allDirections
        `shouldBe` Just <$> "########"
    specify "look NE" $ do
      firstSeatInDirection exampleWith8 (pt (1, -1)) (pt (3, 4))
        `shouldBe` Just '#'
    specify "8 seats visible" $ do
      (length . filter isOccupied . firstSeatsInAnyDirection exampleWith8 $ pt (3, 4))
        `shouldBe` 8

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

part2ExampleWith8 :: Text
part2ExampleWith8 =
  T.unlines
    [ ".......#.",
      "...#.....",
      ".#.......",
      ".........",
      "..#L....#",
      "....#....",
      ".........",
      "#........",
      "...#....."
    ]
