module Main where

import Prelude
import Day1 as Day1
import Day2 as Day2
import Day3 as Day3
import Partial.Unsafe (unsafePartial)

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log "Day 1 - Part 1"
  log $ show Day1.part1
  log $ show Day1.part1'
  log $ show $ unsafePartial Day1.part1''

  log "Day 2 - Part 1"
  log $ show Day2.part1

  log "Day 2 - Part 2"
  log $ show Day2.part2

  log "Day 3 - Part 1"
  log $ show Day3.part1
