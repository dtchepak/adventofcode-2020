module Aoc.Day12Spec (spec) where

import Aoc.Array2D (Point2D (Point2D))
import Aoc.Day12
import qualified Data.Text as T
import Test.Hspec

pt :: (Int, Int) -> Point2D
pt = Point2D

spec :: Spec
spec = do
  describe "part 1" $ do
    specify "F10" $ do
      runAll "F10" `shouldBe` Ship (pt (10, 0)) eastBearing
    specify "F10, N3" $ do
      runAll "F10\nN3" `shouldBe` Ship (pt (10, 3)) eastBearing
    specify "F10, N3, F7" $ do
      runAll "F10\nN3\nF7" `shouldBe` Ship (pt (17, 3)) eastBearing
    specify "F10, N3, F7, R90" $ do
      runAll "F10\nN3\nF7\nR90" `shouldBe` Ship (pt (17, 3)) southBearing
    specify "example journey" $ do
      runAll "F10\nN3\nF7\nR90\nF11" `shouldBe` Ship (pt (17, -8)) southBearing
    specify "dist" $ do
      dist (pt (17, 8)) `shouldBe` 25
    specify "dist with negative values" $ do
      dist (pt (-17, -8)) `shouldBe` 25
    specify "dist for example" $ do
      part1 "F10\nN3\nF7\nR90\nF11" `shouldBe` 25
    specify "180 right" $ do
      right 180 start `shouldBe` Ship (pt (0, 0)) westBearing
    specify "part of day12.txt" $ do
      let instr = T.unlines ["F98", "S4", "S4", "L180", "W4", "S2", "R90", "E4", "F60", "E5", "R180", "F100", "R180", "F92"]
      runAll instr `shouldBe` Ship (pt (103, 42)) northBearing
  describe "part 2" $ do
    specify "1st command" $ do
      runAll' "F10" `shouldBe` WShip (Waypoint (pt (10, 1))) (pt (100, 10))
    specify "2nd command" $ do
      runAll' "F10\nN3" `shouldBe` WShip (Waypoint (pt (10, 4))) (pt (100, 10))
    specify "3rd command" $ do
      runAll' "F10\nN3\nF7" `shouldBe` WShip (Waypoint (pt (10, 4))) (pt (170, 38))
    specify "4th command" $ do
      runAll' "F10\nN3\nF7\nR90" `shouldBe` WShip (Waypoint (pt (4, -10))) (pt (170, 38))
    specify "full example" $ do
      runAll' "F10\nN3\nF7\nR90\nF11" `shouldBe` WShip (Waypoint (pt (4, -10))) (pt (214, -72))
    specify "full example dist" $ do
      part2 "F10\nN3\nF7\nR90\nF11" `shouldBe` 286