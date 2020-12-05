module Aoc.Day02 (
    PasswordPolicy(..),
    PasswordListError(..),
    parsePasswordList,
    parsePasswordListLine,
    isValidPassword,
    isValidPasswordRevised,
    validPasswords,
    part1,
    part2
) where

import Data.Bifunctor (bimap)
import Data.Ix (inRange)
import Data.Functor ( (<&>) )
import qualified Data.Text as T
import Data.Text (Text, count, singleton, split)
import Data.Text.Read (decimal)

data PasswordPolicy = PasswordPolicy { lower :: Int, upper :: Int, char :: Char }
    deriving (Show, Eq)

data PasswordListError
    = InvalidInt Text 
    | InvalidChar Text
    | ExpectedEmpty Text
    | UnrecognisedInput [Text]
    deriving (Show, Eq)

isValidPassword :: PasswordPolicy -> Text -> Bool
isValidPassword p = 
    inRange (lower p, upper p) . count (singleton (char p))

parsePasswordListLine :: Text -> Either PasswordListError (PasswordPolicy, Text)
parsePasswordListLine = 
    let tryReadInt s = bimap (const (InvalidInt s)) fst (decimal s)
        tryReadChar s = if T.length s == 1 then pure (T.head s) else Left (InvalidChar s)
        tryReadEmpty s = if s == T.empty then pure () else Left (ExpectedEmpty s)
        tryReadSegments [a, b, c, d, e] =
            PasswordPolicy <$> tryReadInt a <*> tryReadInt b <*> tryReadChar c <* tryReadEmpty d <&> (,e)
        tryReadSegments other = Left (UnrecognisedInput other)
    in tryReadSegments . split (\s -> s == '-' || s == ' ' || s == ':')

parsePasswordList :: Text -> Either PasswordListError [(PasswordPolicy, Text)]
parsePasswordList = traverse parsePasswordListLine . T.lines

validPasswords :: (PasswordPolicy -> Text -> Bool) -> Text -> Either PasswordListError Int
validPasswords isValid =
    let countValid = length . filter (uncurry isValid)
    in fmap countValid . parsePasswordList

part1 :: Text -> Either PasswordListError Int
part1 = validPasswords isValidPassword

safeIndex :: Text -> Int -> Maybe Char
safeIndex t i =
    if i >= 0 && i < T.length t then pure (T.index t i) else Nothing

isValidPasswordRevised :: PasswordPolicy -> Text -> Bool
isValidPasswordRevised (PasswordPolicy i j c) t =
    let nth = safeIndex t . pred
        xor a b = (a || b) && not (a && b)
    in (nth i == Just c) `xor` (nth j == Just c)

part2 :: Text -> Either PasswordListError Int
part2 = validPasswords isValidPasswordRevised