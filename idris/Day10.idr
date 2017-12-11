module Day10
import Day10_proofs
import Data.Vect
%default total

-- input : Vect 256 Nat
-- input = fromList [0..255]
--
-- lengths : List (Fin 257)
-- lengths = [165, 1, 255, 31, 87, 52, 24, 113,
--            0, 91, 148, 254, 158, 2, 73, 153]

input : Vect 5 Nat
input = fromList [0..4]

lengths : List (Fin 6)
lengths = [3, 4, 1, 5]

rotate : (offset : Nat) -> Vect n a -> Vect n a
rotate _ [] = []
rotate Z xs = xs
rotate {n=(S n')} (S k) (x :: xs) =
  rewrite plusCommutative 1 n' in rotate k (xs ++ [x])

swapKnot : (n : Nat) -> Vect (n + m) a -> Vect (n + m) a
swapKnot n xs = (reverse $ take n xs) ++ (drop n xs)

tieKnot : (offset : Nat)
       -> (len    : Nat)
       -> (chain  : Vect (len + n) a)
       -> Vect (len + n) a
tieKnot offset Z chain = chain
tieKnot {n} offset len@(S k) chain =
   rotate reverseOffset $ swapKnot len $ rotate offset' chain
   where offset' : Nat
         offset' = (modNatNZ offset (n + (S k)) (plusPlusSuccNotZero))

         reverseOffset : Nat
         reverseOffset = (-) (n + (S k)) offset'
           {smaller = believe_me "because of modulo above"}

encrypt : (chain   : Vect n a)
       -> (lengths : List (Fin (S n)))
       -> Vect n a
encrypt = flip (encrypt' 0 0) where
  encrypt' : (offset  : Nat)
          -> (skip    : Nat)
          -> (lengths : List (Fin (S n)))
          -> (chain   : Vect n a)
          -> Vect n a
  encrypt' offset skip [] chain = chain
  encrypt' {n} offset skip (l :: ls) chain =
      encrypt' (offset + skip + lNat) (skip + 1) ls
          -- That's a mess, but without the proofs it calls tieKnot with offset, lNat and chain
          (vectPlusMinusSame lLTEn $
            (tieKnot {n=((-) n lNat {smaller=lLTEn})} -- Cannot be inferred
                     offset
                     lNat
                     (vectSamePlusMinus lLTEn chain)))
    where
      lNat : Nat
      lNat = finToNat l

      lLTEn : LTE (finToNat l) n
      lLTEn = elemSmallerThanBound l

part1encryption : Vect 5 Nat
part1encryption = encrypt input lengths

export
part1 : Nat
part1 = foldl1 (*) $ take 2 $ part1encryption
