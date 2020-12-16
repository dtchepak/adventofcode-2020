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
    runAll',
    WaypointShip (..),
    Waypoint (..),
    part2,
  )
where

import Aoc.Array2D
import Data.List (foldl')
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as T

class Positionable a where
  updatePosition :: (Point2D -> Point2D) -> a -> a

data Ship = Ship
  { position :: Point2D,
    bearing :: Point2D
  }
  deriving (Show, Eq)

instance Positionable Ship where
  updatePosition f (Ship p b) = Ship (f p) b

updateBearing :: (Point2D -> Point2D) -> Ship -> Ship
updateBearing f (Ship p b) = Ship p (f b)

eastBearing :: Point2D
eastBearing = Point2D (1, 0)

westBearing :: Point2D
westBearing = Point2D (-1, 0)

northBearing :: Point2D
northBearing = Point2D (0, 1)

southBearing :: Point2D
southBearing = Point2D (0, -1)

start :: Ship
start = Ship (Point2D (0, 0)) eastBearing

parseCommand :: Text -> Maybe (Text, Int)
parseCommand t =
  let (cmd, arg) = T.splitAt 1 t
      parseInt = fmap fst . either (const Nothing) pure . T.decimal
   in (cmd,) <$> parseInt arg

forward :: Int -> Ship -> Ship
forward x (Ship pos brg) = Ship (pos <> (brg `multiplyPt` x)) brg

north :: Positionable a => Int -> a -> a
north x = updatePosition (<> (northBearing `multiplyPt` x))

south :: Positionable a => Int -> a -> a
south x = north (- x)

east :: Positionable a => Int -> a -> a
east x = updatePosition (<> (eastBearing `multiplyPt` x))

west :: Positionable a => Int -> a -> a
west x = east (- x)

-- | Rotate a point clockwise about the origin a number of degrees (must be a multiple of 90)
-- Clockwise rotation of (x,y): x' = x cos t - y sin t; y' = y cos t + x sin t
rotate :: Positionable a => Int -> a -> a
rotate 90 = updatePosition (\(Point2D (x, y)) -> Point2D (- y, x))
rotate 180 = rotate 90 . rotate 90
rotate 270 = rotate (-90)
rotate (-90) = updatePosition (\(Point2D (x, y)) -> Point2D (y, - x))
rotate (-180) = rotate 180
rotate (-270) = rotate 90
rotate _ = id

left :: Int -> Ship -> Ship
left = updateBearing . rotate

right :: Int -> Ship -> Ship
right = left . negate

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

newtype Waypoint = Waypoint Point2D
  deriving (Show, Eq)

instance Positionable Waypoint where
  updatePosition f (Waypoint p) = Waypoint (f p)

data WaypointShip = WShip Waypoint Point2D
  deriving (Show, Eq)

instance Positionable WaypointShip where
  updatePosition f (WShip w p) = WShip w (f p)

waypointLeft :: Int -> Waypoint -> Waypoint
waypointLeft = updatePosition . rotate

waypointRight :: Int -> Waypoint -> Waypoint
waypointRight = waypointLeft . negate

instance Positionable Point2D where
  updatePosition = id

moveToWaypoint :: Int -> WaypointShip -> WaypointShip
moveToWaypoint i (WShip (Waypoint wp) p) =
  WShip (Waypoint wp) (updatePosition (<> wp `multiplyPt` i) p)

updateWaypoint :: (Waypoint -> Waypoint) -> WaypointShip -> WaypointShip
updateWaypoint f (WShip w p) = WShip (f w) p

navigateWaypoint :: (Text, Int) -> WaypointShip -> WaypointShip
navigateWaypoint (cmd, arg) =
  case cmd of
    "N" -> updateWaypoint (north arg)
    "S" -> updateWaypoint (south arg)
    "E" -> updateWaypoint (east arg)
    "W" -> updateWaypoint (west arg)
    "L" -> updateWaypoint (waypointLeft arg)
    "R" -> updateWaypoint (waypointRight arg)
    "F" -> moveToWaypoint arg
    _ -> id

run' :: [(Text, Int)] -> WaypointShip
run' =
  let initialWaypoint = Waypoint (Point2D (10, 1))
      initialShipPosition = Point2D (0, 0)
   in foldl' (flip navigateWaypoint) (WShip initialWaypoint initialShipPosition)

runAll' :: Text -> WaypointShip
runAll' = run' . mapMaybe parseCommand . T.lines

part2 :: Text -> Int
part2 =
  let position' (WShip _ p) = p
   in dist . position' . runAll'
