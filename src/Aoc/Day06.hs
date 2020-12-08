module Aoc.Day06
  ( part1,
    part2,
  )
where

import Data.Char (isAsciiLower)
import Data.List (intersect, nub)
import Data.Text (Text)
import qualified Data.Text as T

parseGroup :: Text -> [Char]
parseGroup =
  nub . T.unpack . T.filter isAsciiLower

part1 :: Text -> Int
part1 = sum . map (length . parseGroup) . T.splitOn "\n\n"

parseGroup2 :: Text -> [Char]
parseGroup2 = foldr (intersect . T.unpack) ['a'..'z'] . T.lines

part2 :: Text -> Int
part2 = sum . map (length .parseGroup2) . T.splitOn "\n\n"