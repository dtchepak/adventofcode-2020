module Aoc.Array2DSpec
  ( spec,
  )
where

import Aoc.Array2D
import qualified Data.Vector as V
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck (Positive (..))

pt :: (Int, Int) -> Point2D
pt = Point2D

mkArray :: Int -> [a] -> Array2D a
mkArray w = Array2D (Width w) . V.fromList

spec :: Spec
spec = do
  describe "array examples" $ do
    let a = mkArray 4 [1 .. 12]
    {- a:
      | 0  1  2  3  : x
    --------------------
     0| 1  2  3  4
     1| 5  6  7  8
     2| 9 10 11 12
     y|
    -}
    specify "lookup first pt" $ do
      lookupPt a (pt (0, 0)) `shouldBe` Just (1 :: Int)
    specify "lookup (1, 0) pt" $ do
      lookupPt a (pt (1, 0)) `shouldBe` Just 2
    specify "lookup pt bottom right" $ do
      lookupPt a (pt (3, 2)) `shouldBe` Just 12
    specify "missing pt" $ do
      lookupPt a (pt (4, 3)) `shouldBe` Nothing
    specify "point to index" $ do
      pointToIndex (Width 10) (Point2D (1, 1)) `shouldBe` 11
    specify "invalid point to index" $ do
      pointToIndex (Width 10) (Point2D (-1, 1)) `shouldBe` -1
    specify "imap" $ do
      imap (\(Point2D (x, y)) z -> if x == y then z * z else z) a
        `shouldBe` mkArray 4 [1, 2, 3, 4, 5, 36, 7, 8, 9, 10, 121, 12]
    specify "neighbours" $ do
      neighbours (pt (0, 0))
        `shouldBe` pt <$> [(-1, -1), (0, -1), (1, -1), (-1, 0), (1, 0), (-1, 1), (0, 1), (1, 1)]
    specify "lookup pts" $ do
      lookupPts a [pt (0, 0), pt (1, 0), pt (3, 2)] `shouldBe` [Just 1, Just 2, Just 12]
    specify "lookup neighbours" $ do
      lookupPts a (neighbours (pt (2, 1))) `shouldBe` pure <$> [2, 3, 4, 6, 8, 10, 11, 12]
    prop "round trip point" $
      \(Positive width) (Positive x) (Positive y) ->
        let p = pt (x, y)
            w = Width (width + x) -- width should be big enough to accomodate x index
            roundtrip = indexToPoint w . pointToIndex w
         in roundtrip p `shouldBe` p
