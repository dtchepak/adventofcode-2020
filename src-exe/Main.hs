module Main where

import qualified Aoc.Day1 as Day1
import qualified Aoc.Day2 as Day2

import qualified Data.Text.IO as T

main :: IO ()
main = do
  putStrLn "Day 1"
  putStrLn "====="
  print Day1.part1
  print Day1.part2
  putStrLn "Day 2"
  putStrLn "====="
  day2Input <- T.readFile "data/day2.txt"
  print (Day2.part1 day2Input)
  print (Day2.part2 day2Input)
