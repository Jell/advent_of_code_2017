module Day3
%default total

data Dir = Right | Up | Left | Down

target : Nat
target = 361527

dirs : Stream Dir
dirs = cycle $ [Right, Up, Left, Down]

data Coords : Type where
  XY : Int -> Int -> Coords

transition : Dir -> Coords -> Bool
transition Right (XY x y) = x == -y
transition Up    (XY x y) = x == (y + 1)
transition Left  (XY x y) = (x - 1) == -y
transition Down  (XY x y) = x == (y - 1)

step : Dir -> Coords -> Coords
step Right (XY x y) = XY (x + 1) y
step Up    (XY x y) = XY x (y + 1)
step Left  (XY x y) = XY (x - 1) y
step Down  (XY x y) = XY x (y - 1)

coordsIter : Stream Dir -> Coords -> Stream Coords
coordsIter ds c = c :: coordsIter (if (transition dir c) then next else ds) (step dir c)
  where dir = head ds
        next = tail ds

coords : Stream Coords
coords = coordsIter dirs (XY 0 0)

dist : Coords -> Nat
dist (XY x y) = cast $ (abs x) + (abs y)

export
part1 : Nat
part1 = dist $ index target coords
