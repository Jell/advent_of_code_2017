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

withinBounds : Coords -> Coords -> Bool
withinBounds (XY x1 y1) (XY x2 y2) = dx <= 1 && dx >= -1 && dy <= 1 && dy >= -1
  where
    dx : Int
    dx = (x1 - x2)
    dy : Int
    dy = (y1 - y2)

costFromPast : Coords -> List (Coords, Nat) -> Nat
costFromPast c past = sum $ map snd $ filter ((withinBounds c) . fst) past

weightsIter : List (Coords, Nat) -> Stream Coords -> Stream Nat
weightsIter past (newCoords :: nextCoords) = newCost :: weightsIter (newWeight :: past) nextCoords
  where
    newCost : Nat
    newCost = costFromPast newCoords past
    newWeight : (Coords, Nat)
    newWeight = (newCoords, newCost)

weights : Stream Nat
weights = weightsIter [((XY 0 0), 1)] (tail coords)

partial
find : (a -> Bool) -> Stream a -> a
find f (value :: xs) = if f value then value else find f xs

partial export
part2 : Nat
part2 = find ((<=) target) weights
