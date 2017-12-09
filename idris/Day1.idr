module Day1
import FileProvider
%default total
%language TypeProviders

%provide (inputString : String) with readString "Day1_input.txt"

input : String
input = trim inputString

pairs : String -> (String -> Nat) -> List (Char, Char)
pairs i offset = zip chars $ drop (offset i) (chars ++ chars)
  where chars = unpack i

charToNat : Char -> Nat
charToNat c = cast (the String (cast c))

solve : String -> (String -> Nat) -> Nat
solve i offset = foldl (+) 0 $ map (charToNat . fst) $ filter (uncurry (==)) $ pairs i offset

export
part1 : Nat
part1 = solve input (const 1)

export
part2 : Nat
part2 = solve input halfLength
  where
    two : Nat
    two = 2

    twoNotEqNZ : Not (two = 0)
    twoNotEqNZ = case decEq two 0 of
                        (Yes Refl) impossible
                        (No contra) => contra

    halfLength : String -> Nat
    halfLength s = divNatNZ (length s) two twoNotEqNZ
