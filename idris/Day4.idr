module Day4
%default total
%language TypeProviders

readInput : IO (Provider String)
readInput = do
  Right text <- readFile "Day4_input.txt"
        | Left f => pure (Error (show f))
  pure $ Provide text

%provide (input : String) with readInput

noDups : Eq a => List a -> Bool
noDups xs = xs == (nub xs)

export
part1 : Nat
part1 = size $ filter noDups $ map words $ lines input

export
part2 : Nat
part2 = size $ filter noDups $ map (map (sort . unpack)) $ map words $ lines input
