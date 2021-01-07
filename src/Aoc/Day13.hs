module Aoc.Day13
  ( earliest,
    part1,
    part2,
    consecutive,
    part2Basic
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

-- | Check bus arrives at t+i.
tryTimestamp :: Int -> (Index, Int) -> Bool
tryTimestamp t (i, bus) =
  -- If it divides evenly without remainder then it will arrive at this time.
  (t + i) `mod` bus == 0
--  t `mod` bus == (bus - i) `mod` bus

tryTimestampForAll :: Int -> [(Index, Int)] -> Bool
tryTimestampForAll = all . tryTimestamp

consecutive :: Text -> Maybe Int
consecutive txt =
  let input = parse txt
      (indexOfLargest, largestTerm) = maximumBy (compare `on` snd) input
      timestampFor multiplier = largestTerm * multiplier - indexOfLargest
      findMultiplier = find (flip tryTimestampForAll input . timestampFor) [1 ..]
   in timestampFor <$> findMultiplier

-- Too slow for large inputs.
part2Basic :: Maybe Int
part2Basic = consecutive part2Times

type Input = (Index, Int)

data State = State {lcm' :: Int, ts :: Int}
  deriving (Show, Eq)

{-
Once a valid timestamp `ts` is found for a set of inputs, the next valid timestamp will be `ts + lcm`
(where lcm = lowest common multiple of the inputs used for ts).

To work out a new valid timestamp for an additional input, check timestamps that are valid for the existing
inputs (using `ts + lcs`) until one is also compatible with the additional input, then update the state with
the new lcm (https://stackoverflow.com/a/147523/906) and ts.
-}
part2 :: Int
part2 =
  let step :: Input -> State -> State
      step input (State l t) =
        if tryTimestamp t input
          then State (lcm (snd input) l) t
          else step input (State l (l + t))
   in ts $ foldr step (State 1 0) (parse part2Times)