module Aoc.Day04
  ( isValidPassportsText,
    parsePassport,
    parsePassports,
    validatePassportValues,
    parseBirthYear,
    parseIssueYear,
    parseExpiry,
    parseHeight,
    parseHairColour,
    parseEyeColour,
    parseId,
    part2,
    part1,
    Height (..),
    parseAll,
  )
where

import Control.Applicative
import Control.Monad
import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as P
import Data.Char (isHexDigit)
import Data.Either (isRight)
import Data.Functor
import qualified Data.List as List
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

data PassportField = Field Text Text
  deriving (Show, Eq)

key :: PassportField -> Text
key (Field k _) = k

fieldValue :: PassportField -> Text
fieldValue (Field _ v) = v

parsePassport :: Text -> [PassportField]
parsePassport t =
  let fields = T.split (\c -> c == ' ' || c == '\n') t
      field = fmap (T.drop 1) . T.breakOn ":"
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

-- Part 2

failParse :: Text -> Parser a
failParse = fail . T.unpack

inRange :: Ord a => (a, a) -> a -> Parser a
inRange (lo, hi) a = if a >= lo && a <= hi then pure a else failParse "Not in range"

parseBirthYear :: Parser Int
parseBirthYear = P.decimal >>= inRange (1920, 2002)

parseIssueYear :: Parser Int
parseIssueYear = P.decimal >>= inRange (2010, 2020)

parseExpiry :: Parser Int
parseExpiry = P.decimal >>= inRange (2020, 2030)

data Height = Cm Int | Inches Int
  deriving (Show, Eq)

parseHeightUnit :: Int -> Parser Height
parseHeightUnit x = (P.string "cm" $> Cm x) <|> (P.string "in" $> Inches x)

parseHeight :: Parser Height
parseHeight =
  let isInRange h@(Cm x) = inRange (150, 193) x $> h
      isInRange h@(Inches x) = inRange (59, 76) x $> h
   in P.decimal >>= parseHeightUnit >>= isInRange

parseHairColour :: Parser String
parseHairColour = P.char '#' *> replicateM 6 (P.satisfy isHexDigit)

parseEyeColour :: Parser Text
parseEyeColour = P.choice (P.string <$> ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"])

parseId :: Parser String
parseId = replicateM 9 P.digit

parseAll :: Parser a -> Text -> Either String a
parseAll p = P.parseOnly (p <* P.endOfInput)

validatePassportValues :: [PassportField] -> Either String ()
validatePassportValues p =
  let getField k =
        maybe (Left $ "Missing field: " ++ T.unpack k) (pure . fieldValue) . List.find ((== k) . key)
      validateField k v = getField k p >>= parseAll v
      validateField_ k v = validateField k v $> ()
   in sequence_
        [ validateField_ "byr" parseBirthYear,
          validateField_ "iyr" parseIssueYear,
          validateField_ "eyr" parseExpiry,
          validateField_ "hgt" parseHeight,
          validateField_ "hcl" parseHairColour,
          validateField_ "ecl" parseEyeColour,
          validateField_ "pid" parseId
        ]

isValidPassports2 :: Text -> [Either String ()]
isValidPassports2 = fmap validatePassportValues . parsePassports

part2 :: Text -> Int
part2 = length . filter isRight . isValidPassports2