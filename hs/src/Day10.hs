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

knotLens :: Int -> Int -> Simple Lens [a] [a]
knotLens idx l = lens (knotGet idx l) (knotSet idx l)

-- Can't use `[Simpler Lens [a] [a]]` as type because of problems with
-- impredicative polymorphism.
-- See: https://ghc.haskell.org/trac/ghc/wiki/ImpredicativePolymorphism
makeKnotLenses :: forall f a. Functor f => Int -> Int -> Int -> [Int]
               -> [([a] -> f [a]) -> [a] -> f [a]] -- Simple Lens [a] [a]
               -> [([a] -> f [a]) -> [a] -> f [a]] -- Simple Lens [a] [a]
makeKnotLenses idx skip maxIdx [] lenses = reverse lenses
makeKnotLenses idx skip maxIdx (l : ls) lenses =
   makeKnotLenses ((idx + skip + l) `mod` maxIdx)
                  (skip + 1)
                  maxIdx
                  ls
                  ((knotLens idx l) : lenses)

encrypt' :: Int -> Int -> [a] -> [Int] -> [a]
encrypt' idx skip chain lengths =
    makeKnotLenses idx skip (length chain) lengths [] &
    foldl (\c l -> c & l %~ reverse) chain

encrypt :: [a] -> [Int] -> [a]
encrypt = encrypt' 0 0

-- Solution
part1 = foldl (*) 1 $ take 2 $ encrypt input lengths
