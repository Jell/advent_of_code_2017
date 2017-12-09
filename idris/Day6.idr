module Day6
import Data.Vect

blocks : Vect 16 Nat
blocks = [4,1,15,12,0,9,9,5,5,8,7,3,14,5,12,3]

stepIter : {n     : Nat}
        -> (stock : Nat)
        -> (idx   : Fin (S n))
        -> (input : Vect (S n) Nat)
        -> Vect (S n) Nat
stepIter Z idx input = input
stepIter {n} (S stock) idx input =
         stepIter stock nextIdx (updateAt nextIdx S input)
  where
    nextIdx : Fin (S n)
    nextIdx = fromMaybe FZ $ natToFin (S (cast idx)) (S n)


step : {n : Nat} -> Vect (S n) Nat -> Vect (S n) Nat
step {n} xs = stepIter max maxIdx (replaceAt maxIdx 0 xs)
  where
    max : Nat
    max = foldl1 maximum xs
    maxIdx : Fin (S n)
    maxIdx = case elemIndex max xs of
                  Nothing => FZ -- can't happen but whatevs
                  Just x => x

partial
firstDup : Eq a => Stream a -> List a -> List a
firstDup (value :: xs) ys =
  case find (== value) ys of
    Nothing => firstDup xs (value :: ys)
    (Just x) => (value :: ys)

partial
part1dups : List (Vect 16 Nat)
part1dups = firstDup (iterate step blocks) []

partial export
part1 : Nat
part1 = length $ part1dups

partial
dupVal : Maybe (Vect 16 Nat)
dupVal = head' part1dups

partial export
part2 : Maybe Nat
part2 = case dupVal of
             Nothing => Nothing
             (Just dup) => Just $ length $ firstDup (iterate step dup) []
