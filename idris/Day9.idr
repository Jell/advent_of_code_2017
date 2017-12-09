module Day9

import FileProvider
import Lightyear.Core
import Lightyear.Combinators
import Lightyear.Char
import Lightyear.Strings

%language TypeProviders

%provide (inputString : String) with readString "Day9_input.txt"

-- Types
data Tree = Garbage String
          | Group (List Tree)

Show Tree where
  show (Garbage x) = x
  show (Group xs) = show xs

-- Parser
garbageString' : Parser (List Char)
garbageString' = (char '>' *!> pure []) <|> do
  c <- satisfy (/= '>')
  if (c == '!') then anyChar *> garbageString'
                else map (c ::) garbageString'

garbageString : Parser String
garbageString = char '<' *> map pack garbageString'
             <?> "Garbage string"

mutual
  groupList : Parser (List Tree)
  groupList = char '{' *!> (treeValue `sepBy` (char ',')) <* char '}'
             <?> "Group list"

  treeValue : Parser Tree
  treeValue = (map Garbage garbageString)
            <|>| (map Group groupList)

-- Solution

depths : Nat -> Tree -> Nat
depths k (Garbage x) = 0
depths k (Group xs) = k + (sum (map (depths (S k)) xs))

export
part1 : Either String Nat
part1 = depths 1 <$> parse treeValue inputString

measureGarbage : Tree -> Nat
measureGarbage (Garbage x) = length x
measureGarbage (Group xs) = sum $ map measureGarbage xs

export
part2 : Either String Nat
part2 = measureGarbage <$> parse treeValue inputString


-- Tests


test : IO ()
test = do
  putStrLn "depths:"
  printLn $ depths 1 <$> parse treeValue "{}"
  printLn $ depths 1 <$> parse treeValue "{{{}}}"
  printLn $ depths 1 <$> parse treeValue "{{},{}}"
  printLn $ depths 1 <$> parse treeValue "{{{},{},{{}}}}"
  printLn $ depths 1 <$> parse treeValue "{<a>,<a>,<a>,<a>}"
  printLn $ depths 1 <$> parse treeValue "{{<ab>},{<ab>},{<ab>},{<ab>}}"
  printLn $ depths 1 <$> parse treeValue "{{<!!>},{<!!>},{<!!>},{<!!>}}"
  printLn $ depths 1 <$> parse treeValue "{{<a!>},{<a!>},{<a!>},{<ab>}}"
  putStrLn "Garbage:"
  printLn $ measureGarbage <$> parse treeValue "<>"
  printLn $ measureGarbage <$> parse treeValue "<random characters>"
  printLn $ measureGarbage <$> parse treeValue "<<<<>"
  printLn $ measureGarbage <$> parse treeValue "<{!>}>"
  printLn $ measureGarbage <$> parse treeValue "<!!>"
  printLn $ measureGarbage <$> parse treeValue "<!!!>>"
  printLn $ measureGarbage <$> parse treeValue "<{o\"i!a,<{i<a>"
