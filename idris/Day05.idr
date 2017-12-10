module Day05
import FileProvider
import Data.Vect

%default total
%language TypeProviders

%provide (inputString : String) with readString "Day05_short.txt"

input : List String
input = lines inputString

InputSize : Nat
InputSize = length input

initState : Vect InputSize Int
initState = fromList $ map cast input

partial
search : (Int -> Int) -> Nat -> Fin InputSize -> Vect InputSize Int -> Nat
search nextOffset step pos state =
  if nextPos >= 0
  then
    case natToFin (toNat nextPos) InputSize of
      Nothing => nextStep
      (Just nextPosFin) => search nextOffset
                                  nextStep
                                  nextPosFin
                                  (updateAt pos nextOffset state)
  else
    nextStep
  where
    nextStep : Nat
    nextStep = step + 1

    offset : Int
    offset = index pos state

    nextPos : Int
    nextPos = (cast $ finToNat pos) + offset

partial export
part1 : Nat
part1 = search (+ 1) 0 0 initState


incUntil3 : Int -> Int
incUntil3 i = if i < 3
              then i + 1
              else i - 1

partial export
part2 : Nat
part2 = search incUntil3 0 0 initState
