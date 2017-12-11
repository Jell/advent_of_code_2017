module Main
import Data.Vect
import Day01 as Day01
import Day02 as Day02
import Day03 as Day03
import Day04 as Day04
import Day05 as Day05
import Day06 as Day06
import Day07 as Day07
import Day08 as Day08
import Day09 as Day09
import Day10 as Day10

main : IO ()
main = do putStrLn "Day 1"
          printLn Day01.part1
          printLn Day01.part2

          putStrLn "Day 2"
          printLn Day02.part1
          printLn Day02.part2

          putStrLn "Day 3"
          printLn Day03.part1
          printLn Day03.part2

          putStrLn "Day 4"
          printLn Day04.part1
          printLn Day04.part2

          putStrLn "Day 5"
          printLn Day05.part1
          printLn Day05.part2

          putStrLn "Day 6"
          -- Slow, comment out to speedup
          -- printLn Day06.part1
          -- printLn Day06.part2

          putStrLn "Day 7"
          printLn Day07.part1
          putStrLn Day07.part2str

          putStrLn "Day 8"
          printLn Day08.part1
          printLn Day08.part2

          putStrLn "Day 9"
          printLn Day09.part1
          printLn Day09.part2

          putStrLn "Day 10"
          printLn Day10.part1
