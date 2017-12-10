module Day09

import FileProvider
import Lightyear.Core
import Lightyear.Combinators
import Lightyear.Char
import Lightyear.Strings

%language TypeProviders

%provide (inputString : String) with readString "Day09_input.txt"

-- Types
data Stream = Garbage String | Group (List Stream)

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
  groupList : Parser (List Stream)
  groupList = char '{' *!> (treeValue `sepBy` (char ',')) <* char '}'
             <?> "Group list"

  treeValue : Parser Stream
  treeValue = (map Garbage garbageString)
            <|>| (map Group groupList)

-- Solution

depths : Nat -> Stream -> Nat
depths k (Garbage x) = 0
depths k (Group xs) = k + (sum (map (depths (S k)) xs))

export
part1 : Either String Nat
part1 = depths 1 <$> parse treeValue inputString

measureGarbage : Stream -> Nat
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
