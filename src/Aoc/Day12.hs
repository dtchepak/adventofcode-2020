module Aoc.Day12
  ( Ship (..),
    eastBearing,
    westBearing,
    northBearing,
    southBearing,
    start,
    forward,
    left,
    right,
    part1,
    runAll,
    dist,
  )
where

import Aoc.Array2D
import Data.List (foldl')
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as T

data Ship = Ship
  { position :: Point2D,
    bearing :: Point2D
  }
  deriving (Show, Eq)

updatePosition :: (Point2D -> Point2D) -> Ship -> Ship
updatePosition f (Ship p b) = Ship (f p) b

updateBearing :: (Point2D -> Point2D) -> Ship -> Ship
updateBearing f (Ship p b) = Ship p (f b)

eastBearing :: Point2D
eastBearing = Point2D (1, 0)

westBearing :: Point2D
westBearing = Point2D (-1, 0)

northBearing :: Point2D
northBearing = Point2D (0, -1)

southBearing :: Point2D
southBearing = Point2D (0, 1)

start :: Ship
start = Ship (Point2D (0, 0)) eastBearing

parseCommand :: Text -> Maybe (Text, Int)
parseCommand t =
  let (cmd, arg) = T.splitAt 1 t
      parseInt = fmap fst . either (const Nothing) pure . T.decimal
   in (cmd,) <$> parseInt arg

forward :: Int -> Ship -> Ship
forward x (Ship pos brg) = Ship (pos <> (brg `multiplyPt` x)) brg

north :: Int -> Ship -> Ship
north x = updatePosition (<> (northBearing `multiplyPt` x))

south :: Int -> Ship -> Ship
south x = north (- x)

east :: Int -> Ship -> Ship
east x = updatePosition (<> (eastBearing `multiplyPt` x))

west :: Int -> Ship -> Ship
west x = east (- x)

left :: Int -> Ship -> Ship
left 90 =
  updateBearing
    ( \b ->
        if b == northBearing
          then westBearing
          else
            if b == westBearing
              then southBearing
              else
                if b == southBearing
                  then eastBearing
                  else
                    if b == eastBearing
                      then northBearing
                      else b
    )
left 180 = left 90 . left 90
left 270 = right 90
left _ = id

right :: Int -> Ship -> Ship
right 90 =
  updateBearing
    ( \b ->
        if b == northBearing
          then eastBearing
          else
            if b == eastBearing
              then southBearing
              else
                if b == southBearing
                  then westBearing
                  else
                    if b == westBearing
                      then northBearing
                      else b
    )
right 180 = right 90 . right 90
right 270 = left 90
right _ = id

navigate :: (Text, Int) -> Ship -> Ship
navigate (cmd, arg) =
  case cmd of
    "N" -> north arg
    "S" -> south arg
    "E" -> east arg
    "W" -> west arg
    "L" -> left arg
    "R" -> right arg
    "F" -> forward arg
    _ -> id

run :: [(Text, Int)] -> Ship
run = foldl' (flip navigate) start

runAll :: Text -> Ship
runAll = run . mapMaybe parseCommand . T.lines

dist :: Point2D -> Int
dist (Point2D (x, y)) = abs x + abs y

part1 :: Text -> Int
part1 = dist . position . runAll
