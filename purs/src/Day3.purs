module Day3 where

import Prelude

import Data.Generic (class Generic, gShow)
import Data.Ord (abs)
import Data.Tuple (Tuple(..), fst)

data Dir = Right | Up | Left | Down
derive instance genericDir :: Generic Dir

instance showDir :: Show Dir where
  show = gShow

target :: Int
target = 361527

nextDir :: Dir -> Dir
nextDir Right = Up
nextDir Up    = Left
nextDir Left  = Down
nextDir Down  = Right

data Coords = Coords Int Int
derive instance genericCoords :: Generic Coords

instance showCoords :: Show Coords where
  show = gShow

transition :: Dir -> Coords -> Boolean
transition Right (Coords x y) = x == -y
transition Up    (Coords x y) = x == (y + 1)
transition Left  (Coords x y) = (x - 1) == -y
transition Down  (Coords x y) = x == (y - 1)

coordsStep :: Dir -> Coords -> Coords
coordsStep Right (Coords x y) = Coords (x + 1) y
coordsStep Up    (Coords x y) = Coords x (y + 1)
coordsStep Left  (Coords x y) = Coords (x - 1) y
coordsStep Down  (Coords x y) = Coords x (y - 1)

coordsIter :: Int -> (Tuple Coords Dir) -> (Tuple Coords Dir)
coordsIter 0 r = r
coordsIter n (Tuple c dir) = coordsIter (n - 1) (Tuple newCoords newDir)
  where newDir = if (transition dir c)
                 then (nextDir dir)
                 else dir
        newCoords = (coordsStep dir c)


coords :: Int -> Coords
coords n = fst $ coordsIter n (Tuple (Coords 0 0) Right)

dist :: Coords -> Int
dist (Coords x y) = (abs x) + (abs y)

part1 :: Int
part1 = dist $ coords target
