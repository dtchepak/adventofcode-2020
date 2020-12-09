module Aoc.Day07
  ( readBag,
    readRule,
    Bag (..),
    BagRule (..),
    findBagsToContain,
    readRules,
    lookupBag,
    countBagsIn,
    part1,
    part2,
  )
where

-- find bags that can directly hold shiny bag
-- find all bags that can hold those bags
-- ... repeat until no new bags

import Control.Applicative
import Data.Attoparsec.Text
import Data.Functor (($>))
import Data.List (find)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

newtype Bag = Bag Text
  deriving (Show, Eq, Ord)

data BagRule = BagRule Bag [(Int, Bag)]
  deriving (Show, Eq)

bag :: BagRule -> Bag
bag (BagRule x _) = x

carries :: BagRule -> [Bag]
carries = fmap snd . carriesQty

carriesQty :: BagRule -> [(Int, Bag)]
carriesQty (BagRule _ x) = x

word :: Parser Text
word = T.pack <$> many1' letter

space' :: Parser Text
space' = space $> " "

parseBag :: Parser Bag
parseBag =
  Bag . T.concat <$> sequence [word, space', word <* space' <* (string "bags" <|> string "bag")]

parseBagList :: Parser [(Int, Bag)]
parseBagList =
  let bagQty = (,) <$> decimal <*> (space' *> parseBag)
      noBags = string "no other bags" $> []
   in (bagQty `sepBy1'` string ", ") <|> noBags

parseBagRule :: Parser BagRule
parseBagRule = do
  b <- parseBag
  _ <- string " contain "
  bagList <- parseBagList
  _ <- string "."
  pure $ BagRule b bagList

readBag :: Text -> Either String Bag
readBag = parseOnly parseBag

readRule :: Text -> Either String BagRule
readRule = parseOnly parseBagRule

readRules :: Text -> Either String [BagRule]
readRules = traverse readRule . T.lines

findBagsToContain :: [BagRule] -> Bag -> Set Bag
findBagsToContain rules needle =
  let direct = Set.fromList . map bag . filter (elem needle . carries) $ rules
      indirect = Set.unions (Set.map (findBagsToContain rules) direct)
   in direct `Set.union` indirect

part1 :: Text -> Either String Int
part1 =
  fmap (Set.size . flip findBagsToContain (Bag "shiny gold")) . readRules

-- Part 2

lookupBag :: Bag -> [BagRule] -> [(Int, Bag)]
lookupBag needle =
  maybe [] carriesQty . find ((==) needle . bag)

countBagsIn :: Bag -> [BagRule] -> Int
countBagsIn needle rules =
  let direct = lookupBag needle rules
      step (qty, b) = qty + qty * countBagsIn b rules
   in sum (step <$> direct)

part2 :: Text -> Either String Int
part2 = fmap (countBagsIn (Bag "shiny gold")) . readRules