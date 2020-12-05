module Main where

import qualified Aoc.Day01 as Day01
import qualified Aoc.Day02 as Day02

import qualified Data.Text.IO as T

main :: IO ()
main = do
  putStrLn "Day 1"
  putStrLn "====="
  print Day01.part1
  print Day01.part2
  putStrLn "Day 2"
  putStrLn "====="
  day2Input <- T.readFile "data/day2.txt"
  print (Day02.part1 day2Input)
  print (Day02.part2 day2Input)
