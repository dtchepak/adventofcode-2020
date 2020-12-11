{-# LANGUAGE DeriveFunctor #-}

module Aoc.Day10
  ( builtIn,
    joltDiffs,
    part1,
    part2,
    arrangementsHiToLo,
    getArrangements,
  )
where

import Data.List (sort, sortBy)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Read (decimal)

if' :: Bool -> a -> a -> a
if' True = const
if' False = const id

safeMax :: (Ord a) => [a] -> Maybe a
safeMax = if' <$> null <*> const Nothing <*> pure . maximum

builtIn :: [Int] -> Int
builtIn =
  maybe 0 (3 +) . safeMax

type Qty = Int

includeOutletAndBuiltIn :: [Int] -> [Int]
includeOutletAndBuiltIn v = 0 : builtIn v : v

joltDiffs :: [Int] -> Map Int Qty
joltDiffs =
  let diffs sorted = zipWith (-) (drop 1 sorted) sorted
   in foldr
        (Map.alter (Just . maybe 1 succ))
        Map.empty
        . diffs
        . sort
        . includeOutletAndBuiltIn

readInt :: Text -> Maybe Int
readInt = either (const Nothing) (Just . fst) . decimal

part1 :: Text -> Maybe Int
part1 input =
  let values = traverse readInt . T.lines
      diffs = fmap joltDiffs . values $ input
      ones = diffs >>= Map.lookup 1
      threes = diffs >>= Map.lookup 3
   in (*) <$> ones <*> threes

part2 :: Text -> Maybe Int
part2 =
  let values = traverse readInt . T.lines
   in (=<<) getArrangements . values

getArrangements :: [Int] -> Maybe Int
getArrangements =
  getFirst . fmap snd . arrangementsHiToLo . sortBy (flip compare) . (0 :)

data UpToThree a
  = Zero
  | One a
  | Two a a
  | Three a a a
  deriving (Show, Eq, Functor)

getFirst :: UpToThree a -> Maybe a
getFirst = headOr Nothing . fmap pure . threeToList

takeUpToThree :: [a] -> UpToThree a
takeUpToThree (a : b : c : _) = Three a b c
takeUpToThree (a : b : _) = Two a b
takeUpToThree (a : _) = One a
takeUpToThree [] = Zero

push :: a -> UpToThree a -> UpToThree a
push x Zero = One x
push x (One a) = Two x a
push x (Two a b) = Three x a b
push x (Three a b _) = Three x a b

threeToList :: UpToThree a -> [a]
threeToList Zero = []
threeToList (One a) = [a]
threeToList (Two a b) = [a, b]
threeToList (Three a b c) = [a, b, c]

filter3 :: (a -> Bool) -> UpToThree a -> UpToThree a
filter3 p = takeUpToThree . filter p . threeToList

delta :: Int -> Int -> Int
delta x y = if x < y then y - x else x - y

sum3 :: UpToThree Int -> Int
sum3 = sum . threeToList

sum3By :: (a -> Int) -> UpToThree a -> Int
sum3By get = sum3 . fmap get

headOr :: Foldable f => a -> f a -> a
headOr = foldr const

isZero :: UpToThree a -> Bool
isZero Zero = True
isZero _ = False

-- | Work out valid arrangements given a list of adapters
-- sorted from highest to lowest.
arrangementsHiToLo :: [Int] -> UpToThree (Int, Int)
arrangementsHiToLo values =
  let loop :: UpToThree (Int, Int) -> [Int] -> UpToThree (Int, Int)
      loop results [] = results
      loop results (x : xs) =
        let isFirst = isZero results
            compat = filter3 (\(val, _) -> delta x val <= 3) results
            thisResult =
              if isFirst
                then (x, 1) -- always at least one path from final adapter to built-in (due to +3 rule)
                else (x, sum3By snd compat)
         in loop (thisResult `push` results) xs
   in loop Zero values