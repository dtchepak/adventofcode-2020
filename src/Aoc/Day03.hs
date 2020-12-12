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

import Aoc.Array2D
import qualified Data.List.NonEmpty as NEL
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector.Unboxed ((!?))
import qualified Data.Vector.Unboxed as V

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