module Aoc.Day03
  ( TreeLocations,
    Point2D (..),
    Width,
    parseTreeLocations,
    hasTree,
    hasNoTree,
    treeCountOnSlope,
    gradient,
    height,
    width,
    dumpTrees,
    part1,
    part2,
  )
where

import qualified Data.List.NonEmpty as NEL
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector.Unboxed ((!?))
import qualified Data.Vector.Unboxed as V

newtype Width = Width Int
  deriving (Show, Eq, Ord)

newtype Point2D = Point2D (Int, Int)
  deriving (Show, Eq)

instance Semigroup Point2D where
  Point2D (x, y) <> Point2D (x2, y2) = Point2D (x + x2, y + y2)

instance Monoid Point2D where
  mempty = Point2D (0, 0)

data TreeLocations
  = TreeLocations
      Width
      (V.Vector Char)
  deriving (Show, Eq)

width :: TreeLocations -> Int
width (TreeLocations (Width w) _) = w

height :: TreeLocations -> Int
height (TreeLocations (Width w) v) =
  V.length v `div` w

pointToIndex :: Width -> Point2D -> Int
pointToIndex (Width w) (Point2D (x, y)) = x `mod` w + y * w

(??) :: Maybe a -> a -> a
(??) = flip fromMaybe

parseTreeLocations :: Text -> TreeLocations
parseTreeLocations t =
  let columnCount = T.findIndex (== '\n') t ?? T.length t
      flattenedInput = T.filter (/= '\n') t
   in TreeLocations
        (Width columnCount)
        ( V.generate (T.length flattenedInput) (T.index flattenedInput)
        )

hasTree :: TreeLocations -> Point2D -> Bool
hasTree (TreeLocations w v) pt =
  v !? pointToIndex w pt == Just '#'

hasNoTree :: TreeLocations -> Point2D -> Bool
hasNoTree t = not . hasTree t

gradient :: Point2D -> Point2D -> NEL.NonEmpty Point2D
gradient slope =
  NEL.unfoldr (\p -> (p, Just (p <> slope)))

treeCountOnSlope :: TreeLocations -> Point2D -> Int
treeCountOnSlope t p =
  let h = height t
      slope = NEL.take h (gradient p mempty)
   in length . filter id . fmap (hasTree t) $ slope

dumpTrees :: TreeLocations -> [[Char]]
dumpTrees (TreeLocations (Width w) v) =
  let l = V.toList v
      chunk [] = []
      chunk x = take w x : chunk (drop w x)
   in chunk l

part1 :: Text -> Int
part1 = flip treeCountOnSlope (Point2D (3, 1)) . parseTreeLocations

part2 :: Text -> Int
part2 t =
  let slopes = Point2D <$> [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
      count = treeCountOnSlope . parseTreeLocations
   in product (count t <$> slopes)