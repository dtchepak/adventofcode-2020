module Aoc.Day05
  ( binarySpace,
    right,
    left,
    upper,
    lower,
    seatId,
    Tree (..),
    Partition (..),
    PathElement (..),
    mkPartition,
    followPath,
    parseBoardingPasses,
    part1,
    part2,
  )
where

import Data.List (find, maximumBy, sortOn)
import Data.Maybe (catMaybes)
import Data.Monoid
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as T

data Partition
  = Range !Int !Int
  | Value !Int
  deriving (Show, Eq)

mkPartition :: Int -> Int -> Partition
mkPartition lo hi
  | lo > hi || lo < 0 = Value 0
  | lo == hi = Value lo
  | otherwise = Range lo hi

splitPartition :: Partition -> (Partition, Partition)
splitPartition (Range lo hi) =
  let half = (hi - lo) `div` 2
   in (mkPartition lo (lo + half), mkPartition (lo + half + 1) hi)
splitPartition v@(Value _) = (v, v)

upper :: Partition -> Partition
upper = snd . splitPartition

lower :: Partition -> Partition
lower = fst . splitPartition

data Tree a
  = Node (Tree a) (Tree a)
  | Leaf a
  deriving (Show, Eq)

left :: Tree a -> Tree a
left (Node x _) = x
left x@(Leaf _) = x

right :: Tree a -> Tree a
right (Node _ x) = x
right x@(Leaf _) = x

forwardCompose :: [a -> a] -> a -> a
forwardCompose = appEndo . getDual . foldMap (Dual . Endo)

data PathElement = LeftPath | RightPath deriving (Show, Eq)

followPath :: [PathElement] -> Tree a -> Tree a
followPath =
  let action LeftPath = left
      action RightPath = right
   in forwardCompose . fmap action

binarySpace :: Int -> Int -> Tree Partition
binarySpace lo hi =
  let mkTree v@(Value _) = Leaf v
      mkTree r@(Range _ _) =
        let (lower', upper') = splitPartition r
         in Node (mkTree lower') (mkTree upper')
   in mkTree (mkPartition lo hi)

seatId :: Int -> Int -> Int
seatId r c = r * 8 + c

parsePath :: Text -> [PathElement]
parsePath =
  let mapChar c
        | c == 'F' = LeftPath
        | c == 'L' = LeftPath
        | otherwise = RightPath -- 'B', 'R'
   in T.foldr ((:) . mapChar) []

data BoardingPass = BoardingPass
  { row :: !Int,
    col :: !Int,
    seat :: !Int
  }
  deriving (Show, Eq)

mkBoardingPass :: Int -> Int -> BoardingPass
mkBoardingPass r c = BoardingPass r c (seatId r c)

rows :: Tree Partition
rows = binarySpace 0 127

cols :: Tree Partition
cols = binarySpace 0 7

parseBoardingPass :: Text -> Maybe BoardingPass
parseBoardingPass s =
  let (rowText, colText) = T.splitAt 7 s
      rowPath = followPath . parsePath $ rowText
      colPath = followPath . parsePath $ colText
      getValue (Leaf (Value v)) = pure v
      getValue _ = Nothing
   in mkBoardingPass <$> getValue (rowPath rows) <*> getValue (colPath cols)

parseBoardingPasses :: Text -> [Maybe BoardingPass]
parseBoardingPasses = fmap parseBoardingPass . T.lines

part1 :: Text -> BoardingPass
part1 = maximumBy (comparing seat) . catMaybes . parseBoardingPasses

part2 :: Text -> Maybe (BoardingPass, BoardingPass)
part2 =
  let sorted = sortOn seat . catMaybes . parseBoardingPasses
      diffs x = zip x (drop 1 x)
   in find (\(x, y) -> seat y - seat x > 1) . diffs . sorted