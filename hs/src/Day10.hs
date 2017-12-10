{-# LANGUAGE Rank2Types #-}
module Day10 where
import Control.Lens

input :: [Int]
input = [0..255]

lengths :: [Int]
lengths = [165,1,255,31,87,52,24,113,0,91,148,254,158,2,73,153]

knotGet :: Int -> Int -> [a] -> [a]
knotGet idx l chain =
    let chainLength = (length chain) in
    take l $ drop idx $ cycle chain

knotSet :: Int -> Int -> [a] -> [a] -> [a]
knotSet idx l chain sub =
    let chainLength = (length chain) in
    cycle chain &
    drop idx &
    drop l &
    (++) sub &
    take chainLength &
    cycle &
    drop (chainLength - idx) &
    take chainLength

-- type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t
knotLens :: Int -> Int -> Lens [a] [a] [a] [a]
knotLens idx l = lens (knotGet idx l) (knotSet idx l)

encrypt' :: Int -> Int -> [a] -> [Int] -> [a]
encrypt' idx skip chain [] = chain
encrypt' idx skip chain (l : ls) =
    encrypt' ((idx + skip + l) `mod` (length chain))
            (skip + 1)
            (chain & (knotLens idx l) %~ reverse)
            ls

encrypt :: [a] -> [Int] -> [a]
encrypt = encrypt' 0 0

-- Solution
part1 = foldl (*) 1 $ take 2 $ encrypt input lengths
