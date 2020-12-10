module Aoc.Day10 (builtIn, joltDiffs, part1) where

import Data.List (sort)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Read (decimal)

if' :: Bool -> a -> a -> a
if' True = const
if' False = const id

safeMax :: (Ord a) => [a] -> Maybe a
safeMax = if' <$> null <*> const Nothing <*> pure . maximum

builtIn :: [Int] -> Int
builtIn =
  maybe 0 (3 +) . safeMax

type Qty = Int

joltDiffs :: [Int] -> Map Int Qty
joltDiffs =
  let includeOutlet = (:) 0
      includeBuiltIn v = builtIn v : v
      diffs sorted = zipWith (-) (drop 1 sorted) sorted
   in foldr
        (Map.alter (maybe (Just 1) (Just . succ)))
        Map.empty
        . diffs
        . sort
        . includeBuiltIn
        . includeOutlet

readInt :: Text -> Maybe Int
readInt = either (const Nothing) (Just . fst) . decimal

part1 :: Text -> Maybe Int
part1 input =
  let values = traverse readInt . T.lines
      diffs = fmap joltDiffs . values $ input
      ones = diffs >>= Map.lookup 1
      threes = diffs >>= Map.lookup 3
   in (*) <$> ones <*> threes