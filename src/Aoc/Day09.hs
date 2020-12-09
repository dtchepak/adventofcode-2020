module Aoc.Day09
  ( findFirstInvalid,
    Result (..),
    part1,
    findContiguousSum,
    contiguousSum,
    searchSlice,
    part2,
  )
where

import Control.Applicative
import Data.Functor (($>))
import Data.List (elem)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Read (decimal)
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V

type PreambleSize = Int

type Values = Vector Int

data Input = Input PreambleSize Values
  deriving (Show, Eq)

newtype State = State {index :: Int}
  deriving (Show, Eq)

next :: State -> State
next (State i) = State (i + 1)

data Result
  = NotEnoughPreamble
  | OutOfInput
  | Invalid Int
  deriving (Show, Eq)

isValid :: [Int] -> Int -> Bool
isValid preamble value = case preamble of
  [] -> False
  (x : xs) ->
    let sums = (x +) <$> xs
     in value `elem` sums || isValid xs value

preamblePlusNext :: Input -> State -> Either Result ([Int], Int)
preamblePlusNext (Input preambleSize values) (State i) =
  let (preamble, rest) = V.splitAt preambleSize (V.drop i values)
   in if V.length preamble < preambleSize
        then Left NotEnoughPreamble
        else
          maybe
            (Left OutOfInput)
            (pure . (,) (V.toList preamble))
            (rest V.!? 0)

step :: Input -> State -> Either Result State
step input state =
  let checkValid (p, v) = if isValid p v then pure () else Left (Invalid v)
   in (preamblePlusNext input state >>= checkValid) $> next state

findFirstInvalid :: Int -> Vector Int -> Result
findFirstInvalid preambleSize values =
  let input = Input preambleSize values
      run (Right s) = run (step input s)
      run (Left r) = r
   in run . pure . State $ 0

readInt :: Text -> Maybe Int
readInt = either (const Nothing) (pure . fst) . decimal

part1 :: Text -> Maybe Result
part1 =
  let input = fmap V.fromList . traverse readInt . T.lines
   in fmap (findFirstInvalid 25) . input

contiguousSum :: Int -> Vector Int -> Int
contiguousSum n = V.sum . V.take n

searchSlice :: Int -> Vector Int -> Maybe (Vector Int)
searchSlice needle haystack =
  foldr
    ( \n acc ->
        let csum = contiguousSum n haystack
         in if csum == needle
              then Just (V.slice 0 n haystack)
              else
                if csum > needle
                  then Nothing
                  else acc
    )
    Nothing
    [2 .. V.length haystack - 1] -- At least two elements, at most every element

findContiguousSum :: Int -> Vector Int -> Maybe (Vector Int)
findContiguousSum needle haystack =
  searchSlice needle haystack <|> findContiguousSum needle (V.drop 1 haystack)

part2 :: Text -> Maybe Int
part2 t =
  let input = fmap V.fromList . traverse readInt . T.lines
      needle = 31161678 -- from part 1
      result = input t >>= findContiguousSum needle
      addMinMax x = V.minimum x + V.maximum x
   in addMinMax <$> result