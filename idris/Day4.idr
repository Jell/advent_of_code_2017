module Day4
import FileProvider

%default total
%language TypeProviders

%provide (input : String) with readString "Day4_input.txt"

noDups : Eq a => List a -> Bool
noDups xs = xs == (nub xs)

export
part1 : Nat
part1 = size $ filter noDups $ map words $ lines input

export
part2 : Nat
part2 = size $ filter noDups $ map (map (sort . unpack)) $ map words $ lines input
