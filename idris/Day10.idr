module Day10
import Data.Vect
%default total

input : Vect 5 Int
input = fromList [0..4]

lengths : List (Fin 6)
-- lengths = [165, 1, 255, 31, 87, 52, 24, 113,
--            0, 91, 148, 254, 158, 2, 73, 153]
lengths = [3, 4, 1, 5]

it'sfine : {len : Nat} -> Vect (len + 1) a -> Vect (S len) a
it'sfine {len} xs = rewrite plusCommutative 1 len in xs

rotate : (offset : Nat) -> Vect n a -> Vect n a
rotate Z xs = xs
rotate (S k) [] = []
rotate (S k) (x :: xs) = it'sfine $ xs ++ [x]

swapKnot : (n : Nat) -> Vect (n + m) a -> Vect (n + m) a
swapKnot n xs = (reverse $ take n xs) ++ (drop n xs)

plusPlusSuccNotZero : ((n + (S k)) = 0) -> Void
plusPlusSuccNotZero {n} {k} = rewrite plusCommutative n (S k) in SIsNotZ

tieKnot : (offset : Nat) -> (len : Nat) -> Vect (len + n) a -> Vect (len + n) a
tieKnot offset Z xs = xs
tieKnot {n} offset len@(S k) xs = rotate reverseOffset $ swapKnot len $ rotate offset xs
   where reverseOffset : Nat
         reverseOffset = (modNatNZ (n + (S k) + offset) (n + (S k)) (plusPlusSuccNotZero))

vectPlusMinusSame : LTE l n -> Vect (l + (n - l)) a -> Vect n a
vectPlusMinusSame {l = Z} {n = Z} prf [] = []
vectPlusMinusSame {l = Z} {n = (S k)} prf (x :: xs) = x :: xs
vectPlusMinusSame {l = (S _)} {n = Z} LTEZero _ impossible
vectPlusMinusSame {l = (S _)} {n = Z} (LTESucc _) _ impossible
vectPlusMinusSame {l = (S k)} {n = (S j)} (LTESucc x) (y :: xs) = y :: vectPlusMinusSame x xs

vectSamePlusMinus : LTE l n -> Vect n a -> Vect (l + (n - l)) a
vectSamePlusMinus {l = Z} {n = Z} x xs = xs
vectSamePlusMinus {l = Z} {n = (S k)} x xs = xs
vectSamePlusMinus {l = (S _)} {n = Z} LTEZero _ impossible
vectSamePlusMinus {l = (S _)} {n = Z} (LTESucc _) _ impossible
vectSamePlusMinus {l = (S k)} {n = (S j)} (LTESucc x) (y :: xs) = y :: vectSamePlusMinus x xs

elemSmallerThanBound : (n : Fin (S m)) -> LTE (finToNat n) m
elemSmallerThanBound {m = Z} FZ = LTEZero
elemSmallerThanBound {m = Z} (FS FZ) impossible
elemSmallerThanBound {m = Z} (FS (FS _)) impossible
elemSmallerThanBound {m = (S k)} FZ = LTEZero
elemSmallerThanBound {m = (S k)} (FS x) = LTESucc (elemSmallerThanBound x)

encrypt : (chain : Vect n a) -> (lengths : List (Fin (S n))) -> Vect n a
encrypt = encrypt' 0 0 where
  encrypt' : (offset : Nat) -> (skip : Nat) -> (chain : Vect n a) -> (lengths : List (Fin (S n))) -> Vect n a
  encrypt' offset skip chain [] = chain
  encrypt' {n} offset skip chain (l :: lens) =
      encrypt' (offset + skip + lNat) (skip + 1)
          (vectPlusMinusSame lLTEn $ (tieKnot {n=((-) n lNat {smaller=lLTEn})} offset lNat (vectSamePlusMinus lLTEn chain)))
          lens
    where
      lNat : Nat
      lNat = finToNat l

      lLTEn : LTE (finToNat l) n
      lLTEn = elemSmallerThanBound l
