module Aoc.Day04
  ( isValidPassportsText,
  part1)
where

import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

data PassportField = Field Text Text
  deriving (Show, Eq)

key :: PassportField -> Text
key (Field k _) = k

parsePassport :: Text -> [PassportField]
parsePassport t =
  let fields = T.split (\c -> c == ' ' || c == '\n') t
      field = T.breakOn ":"
   in uncurry Field . field <$> fields

parsePassports :: Text -> [[PassportField]]
parsePassports = fmap parsePassport . T.splitOn "\n\n"

isValidPassport :: [PassportField] -> Bool
isValidPassport p =
  let requiredFields =
        Set.fromList
          [ "byr", -- (Birth Year)
            "iyr", -- (Issue Year)
            "eyr", -- (Expiration Year)
            "hgt", -- (Height)
            "hcl", -- (Hair Color)
            "ecl", -- (Eye Color)
            "pid" -- (Passport ID)
            -- , "cid" -- (Country ID)
          ]
   in requiredFields `Set.isSubsetOf` Set.fromList (key <$> p)

isValidPassportsText :: Text -> [Bool]
isValidPassportsText = fmap isValidPassport . parsePassports

part1 :: Text -> Int
part1 = length . filter id . isValidPassportsText