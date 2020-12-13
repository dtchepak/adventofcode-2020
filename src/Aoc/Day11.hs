module Aoc.Day11
  ( importArray,
    gen,
    run,
    countOccupied,
    isOccupied,
    updatePosition,
    adjacentSeats,
    part1,
    gen',
    run',
    part2,
    allDirections,
    firstSeatInDirection,
    firstSeatsInAnyDirection,
    ptsInDirection,
  )
where

import Aoc.Array2D
import qualified Data.List as List
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V

importArray :: Text -> Array2D Char
importArray t =
  let width = fromMaybe 0 . T.findIndex (== '\n') $ t
      chars = V.fromList . T.unpack . T.filter (/= '\n') $ t
   in Array2D (Width width) chars

occupied :: Char
occupied = '#'

empty :: Char
empty = 'L'

isOccupied :: Char -> Bool
isOccupied = (== occupied)

isEmpty :: Char -> Bool
isEmpty = (== empty)

filledCount :: [Char] -> Int
filledCount = length . filter isOccupied

-- | Get list of values adjacent to the given position
adjacentSeats :: Array2D a -> Point2D -> [a]
adjacentSeats a = catMaybes . lookupPts a . neighbours

updatePosition :: Array2D Char -> Point2D -> Char -> Char
updatePosition a pt pos =
  let filledAdj = filledCount (adjacentSeats a pt)
   in if isEmpty pos && filledAdj == 0
        then occupied
        else
          if isOccupied pos && filledAdj >= 4
            then empty
            else pos

gen :: Array2D Char -> Array2D Char
gen a =
  imap (updatePosition a) a

run :: Array2D Char -> Array2D Char
run input =
  let next = gen input
   in if next == input then input else run next

countOccupied :: Array2D Char -> Int
countOccupied (Array2D _ v) = V.length . V.filter isOccupied $ v

part1 :: Text -> Int
part1 = countOccupied . run . importArray

ptsInDirection :: Array2D Char -> Point2D -> Point2D -> [Point2D]
ptsInDirection a dirDiff =
  List.unfoldr
    ( \p ->
        let p' = p <> dirDiff
         in if inBounds a p' then Just (p', p') else Nothing
    )

safeHead :: [a] -> Maybe a
safeHead = foldr (const . Just) Nothing

firstSeatInDirection :: Array2D Char -> Point2D -> Point2D -> Maybe Char
firstSeatInDirection a dirDiff =
  safeHead
    . List.filter (\c -> isOccupied c || isEmpty c)
    . catMaybes
    . lookupPts a
    . ptsInDirection a dirDiff

allDirections :: [Point2D]
allDirections =
  Point2D
    <$> [ (-1, -1), -- NW
          (0, -1), -- N
          (1, -1), -- NE
          (-1, 0), -- W
          (1, 0), -- E
          (-1, 1), -- SW
          (0, 1), -- S
          (1, 1) -- SE
        ]

firstSeatsInAnyDirection :: Array2D Char -> Point2D -> [Char]
firstSeatsInAnyDirection a pt =
  catMaybes (flip (firstSeatInDirection a) pt <$> allDirections)

updatePosition' :: Array2D Char -> Point2D -> Char -> Char
updatePosition' a pt pos =
  let seatsFilled = filledCount (firstSeatsInAnyDirection a pt)
   in if isEmpty pos && seatsFilled == 0
        then occupied
        else
          if isOccupied pos && seatsFilled >= 5
            then empty
            else pos

gen' :: Array2D Char -> Array2D Char
gen' a =
  imap (updatePosition' a) a

run' :: Array2D Char -> Array2D Char
run' input =
  let next = gen' input
   in if next == input then input else run' next

part2 :: Text -> Int
part2 = countOccupied . run' . importArray