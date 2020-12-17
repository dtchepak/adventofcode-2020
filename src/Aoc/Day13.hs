module Aoc.Day13
  ( earliest,
    part1,
    part2,
    consecutive,
  )
where

import Data.Function (on)
import Data.List (find, maximumBy, sortOn)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as T

-- | Return the next timestamp after `time` that a particular bus will arrive.
nextFromTime :: Int -> Int -> Int
nextFromTime time busId =
  let numberOfTripsToGetToTime = ceiling (fromIntegral time / fromIntegral busId)
   in busId * numberOfTripsToGetToTime

safeHead :: [a] -> Maybe a
safeHead = foldr (const . pure) Nothing

earliest :: Int -> [Int] -> Maybe Int
earliest time =
  safeHead . sortOn (nextFromTime time)

part1Timestamp :: Int
part1Timestamp = 1013728

part1Timetable :: [Int]
part1Timetable = [23, 41, 733, 13, 17, 19, 29, 449, 37]

part1 :: Maybe Int
part1 =
  let ts = part1Timestamp
      timetable = part1Timetable
      bus = earliest ts timetable
      minutes = (\t -> t - ts) . nextFromTime ts <$> bus
   in (*) <$> bus <*> minutes

part2Times :: Text
part2Times = "23,x,x,x,x,x,x,x,x,x,x,x,x,41,x,x,x,x,x,x,x,x,x,733,x,x,x,x,x,x,x,x,x,x,x,x,13,17,x,x,x,x,19,x,x,x,x,x,x,x,x,x,29,x,449,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,37"

type Index = Int

parse :: Text -> [(Index, Int)]
parse =
  let parseInt = either (const 0) fst . T.decimal
   in filter ((/= 0) . snd) . fmap (parseInt <$>) . zip [0 ..] . T.split (== ',')

-- | Check bus arrives at ts+i.
tryTimestamp :: Int -> (Index, Int) -> Bool
tryTimestamp ts (i, bus) =
  -- If it divides evenly without remainder then it will arrive at this time.
  (ts + i) `mod` bus == 0

tryTimestampForAll :: Int -> [(Index, Int)] -> Bool
tryTimestampForAll ts = all (tryTimestamp ts)

consecutive :: Text -> Maybe Int
consecutive txt =
  let input = parse txt
      (indexOfLargest, largestTerm) = maximumBy (compare `on` snd) input
      timestampFor multiplier = largestTerm * multiplier - indexOfLargest
      findMultiplier = find (flip tryTimestampForAll input . timestampFor) [1 ..]
   in timestampFor <$> findMultiplier

--  in findMultiplier

part2 :: Maybe Int
part2 = consecutive part2Times