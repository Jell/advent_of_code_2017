module Day10_proofs
import Data.Vect
%default total
%access export

PlusPlusSuccNotZero : Not ((n + (S k)) = 0)
PlusPlusSuccNotZero {n} {k} = rewrite plusCommutative n (S k) in SIsNotZ

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
