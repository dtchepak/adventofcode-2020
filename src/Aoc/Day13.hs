module Aoc.Day13
  ( earliest,
    part1,
  )
where

import Data.Attoparsec.Text
import Data.List (sortOn)
import Data.Text (Text)
import qualified Data.Text as T

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
  let
    ts = part1Timestamp
    timetable = part1Timetable
    bus = earliest ts timetable
    minutes = (\t -> t - ts) . nextFromTime ts <$> bus
  in (*) <$> bus <*> minutes