module Main
import Day1 as Day1
import Day2 as Day2
import Day3 as Day3
import Day4 as Day4
import Day5 as Day5

main : IO ()
main = do putStrLn "Day 1"
          printLn Day1.part1
          printLn Day1.part2

          putStrLn "Day 2"
          printLn Day2.part1
          printLn Day2.part2

          putStrLn "Day 3"
          printLn Day3.part1
          printLn Day3.part2

          putStrLn "Day 4"
          printLn Day4.part1
          printLn Day4.part2

          putStrLn "Day 5"
          printLn Day5.part1
          printLn Day5.part2
