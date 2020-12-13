module Main where

import qualified Aoc.Day01 as Day01
import qualified Aoc.Day02 as Day02
import qualified Aoc.Day03 as Day03
import qualified Aoc.Day04 as Day04
import qualified Aoc.Day05 as Day05
import qualified Aoc.Day06 as Day06
import qualified Aoc.Day07 as Day07
import qualified Aoc.Day08 as Day08
import qualified Aoc.Day09 as Day09
import qualified Aoc.Day10 as Day10
import qualified Aoc.Day11 as Day11
import qualified Data.Text.IO as T

main :: IO ()
main = do
  putStrLn "Day 11"
  putStrLn "======"
  day11Input <- T.readFile "data/day11.txt"
  print $ Day11.part1 day11Input
  print $ Day11.part2 day11Input


previous :: IO ()
previous = do
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
  print (Day04.part2 day04Input)
  putStrLn "Day 5"
  putStrLn "======"
  day05Input <- T.readFile "data/day05.txt"
  print (Day05.part1 day05Input)
  print (Day05.part2 day05Input)
  putStrLn "Day 6"
  putStrLn "======"
  day06Input <- T.readFile "data/day06.txt"
  print (Day06.part1 day06Input)
  print (Day06.part2 day06Input)
  putStrLn "Day 7"
  putStrLn "======"
  day07Input <- T.readFile "data/day07.txt"
  print (Day07.part1 day07Input)
  print (Day07.part2 day07Input)
  putStrLn "Day 8"
  putStrLn "======"
  day08Input <- T.readFile "data/day08.txt"
  print $ Day08.part1 day08Input
  print $ Day08.part2 day08Input
  putStrLn "Day 9"
  putStrLn "======"
  day09Input <- T.readFile "data/day09.txt"
  print $ Day09.part1 day09Input
  print $ Day09.part2 day09Input
  putStrLn "Day 10"
  putStrLn "======"
  day10Input <- T.readFile "data/day10.txt"
  print $ Day10.part1 day10Input
  print $ Day10.part2 day10Input
