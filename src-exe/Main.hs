module Main where

import qualified Aoc.Day01 as Day01
import qualified Aoc.Day02 as Day02
import qualified Aoc.Day03 as Day03
import qualified Aoc.Day04 as Day04

import qualified Data.Text.IO as T

main :: IO ()
main = do
  putStrLn "Day 1"
  putStrLn "======"
  print Day01.part1
  print Day01.part2
  putStrLn "Day 2"
  putStrLn "======"
  day02Input <- T.readFile "data/day02.txt"
  print (Day02.part1 day02Input)
  print (Day02.part2 day02Input)
  putStrLn "Day 3"
  putStrLn "======"
  day03Input <- T.readFile "data/day03.txt"
  print (Day03.part1 day03Input)
  print (Day03.part2 day03Input)
  putStrLn "Day 4"
  putStrLn "======"
  day04Input <- T.readFile "data/day04.txt"
  print (Day04.part1 day04Input)
