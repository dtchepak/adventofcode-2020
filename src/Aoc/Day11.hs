module Aoc.Day11
  ( importArray,
    gen,
    run,
    countOccupied,
    updatePosition,
    adjacentSeats,
  part1)
where

import Aoc.Array2D
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