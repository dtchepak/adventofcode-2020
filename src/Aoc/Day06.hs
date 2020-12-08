module Aoc.Day06
  (
  part1)
where

import Data.Char (isAsciiLower)
import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text as T

parseGroup :: Text -> [Char]
parseGroup =
  nub . T.unpack . T.filter isAsciiLower

part1 :: Text -> Int
part1 = sum . map (length . parseGroup) . T.splitOn "\n\n"