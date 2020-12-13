module Aoc.Array2D
  ( Width (..),
    Point2D (..),
    Array2D (..),
    pointToIndex,
    indexToPoint,
    lookupPt,
    neighbours,
    lookupPts,
    imap,
    cols,
    rows,
    inBounds,
    multiplyPt,
  )
where

import Data.Vector (Vector)
import qualified Data.Vector as V

newtype Width = Width Int
  deriving (Show, Eq, Ord)

newtype Point2D = Point2D (Int, Int)
  deriving (Show, Eq)

instance Semigroup Point2D where
  Point2D (x, y) <> Point2D (x2, y2) = Point2D (x + x2, y + y2)

instance Monoid Point2D where
  mempty = Point2D (0, 0)

multiplyPt :: Point2D -> Int -> Point2D
multiplyPt (Point2D (x, y)) m = Point2D (m * x, m * y)

data Array2D a = Array2D Width (Vector a)
  deriving (Show, Eq)

instance Functor Array2D where
  fmap f (Array2D w v) = Array2D w (V.map f v)

-- | Map 2D point to an index on a 1D collection with a fixed row size.
-- Both x and y in Point2D must be >=0. If x >= width it will wrap back to 0.
pointToIndex :: Width -> Point2D -> Int
pointToIndex (Width w) (Point2D (x, y)) =
  let rowOffset = y * w
      colOffset = x `mod` w
   in if x < 0 || y < 0 then -1 else rowOffset + colOffset

xOutOfBounds :: Point2D -> Width -> Bool
xOutOfBounds (Point2D (x, _)) (Width w) =
  x < 0 || x >= w

lookupPt :: Array2D a -> Point2D -> Maybe a
lookupPt (Array2D w v) p =
  if p `xOutOfBounds` w
    then Nothing
    else (V.!?) v . pointToIndex w $ p

lookupPts :: Array2D a -> [Point2D] -> [Maybe a]
lookupPts a = fmap (lookupPt a)

pt :: (Int, Int) -> Point2D
pt = Point2D

indexToPoint :: Width -> Int -> Point2D
indexToPoint (Width w) =
  let flipPt (a, b) = (b, a)
   in pt . flipPt . (`divMod` w)

imap :: (Point2D -> a -> b) -> Array2D a -> Array2D b
imap f (Array2D w a) =
  Array2D w (V.imap (f . indexToPoint w) a)

cols :: Array2D a -> Int
cols (Array2D (Width w) _) = w

rows :: Array2D a -> Int
rows (Array2D w v) =
  let Point2D (_, y) = indexToPoint w (V.length v)
   in y

inBounds :: Array2D a -> Point2D -> Bool
inBounds a (Point2D (x, y)) =
  x >= 0 && x < cols a && y >= 0 && y < rows a

neighbours :: Point2D -> [Point2D]
neighbours (Point2D (x, y)) =
  [ -- row above
    pt (x - 1, y - 1),
    pt (x + 0, y - 1),
    pt (x + 1, y - 1),
    -- same row
    pt (x - 1, y + 0),
    -- pt (x, y)
    pt (x + 1, y + 0),
    -- row below
    pt (x - 1, y + 1),
    pt (x + 0, y + 1),
    pt (x + 1, y + 1)
  ]