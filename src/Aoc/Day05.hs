module Aoc.Day05
  ( seatId,
    parseRowAndCol,
    part1,
    part2,
  )
where

import Data.Bifunctor (Bifunctor (bimap))
import Data.Bits
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (sort)
import Data.Foldable (find)

bitsToInt :: (Char -> Int) -> Text -> Int
bitsToInt toBit =
  T.foldl' (\acc -> (.|.) (shift acc 1) . toBit) zeroBits

seatId :: Int -> Int -> Int
seatId r c = r * 8 + c

parseRowAndCol :: Text -> (Int, Int)
parseRowAndCol =
  let isOn c = if c == 'B' || c == 'R' then 1 else 0 -- Back and Right correspond to bit on/1.
      map2 f = bimap f f
  in map2 (bitsToInt isOn) . T.splitAt 7

passes :: Text -> [Int]
passes = fmap (uncurry seatId . parseRowAndCol) . T.lines

part1 :: Text -> Int
part1 = maximum . passes

part2 :: Text -> Maybe Int
part2 =
  let sorted = sort . passes
      diffs x = zip x (drop 1 x)
      gap = find (\(x, y) -> y - x > 1) . diffs . sorted
  in fmap (succ . fst) . gap