module Day08
import FileProvider
import Control.Monad.State

%default total
%language TypeProviders

%provide (inputString : String) with readString "Day08_input.txt"

input : List String
input = lines inputString

-- Types
data Operation = INC | DEC

data Check = LT | GT | LTE | GTE | EQ | NEQ

data Instruction : Type where
  MkInstruction : (target : String)
                -> (op : Operation)
                -> (opVal : Int)
                -> (checkRef : String)
                -> (checkOp : Check)
                -> (checkVal : Int)
                -> Instruction

-- Parser
parseName : String -> Either String String
parseName "" = Left "Empty name"
parseName s = Right s

parseOp : String -> Either String Operation
parseOp "inc" = Right INC
parseOp "dec" = Right DEC
parseOp s = Left $ "Can't parse op: " ++ s

parseInt : String -> Either String Int
parseInt s = Right (cast s) -- meh

parseCheck : String -> Either String Check
parseCheck "<"  = Right LT
parseCheck ">"  = Right GT
parseCheck "<=" = Right LTE
parseCheck ">=" = Right GTE
parseCheck "==" = Right EQ
parseCheck "!=" = Right NEQ
parseCheck s = Left $ "Can't parse check: "++s

parseLine : String -> Either String Instruction
parseLine s =
  case words s of
    (ts :: os :: ovs :: _ :: crs :: cos :: cvs :: []) =>
      MkInstruction
        <$> parseName ts
        <*> parseOp os
        <*> parseInt ovs
        <*> parseName crs
        <*> parseCheck cos
        <*> parseInt cvs
    _ => Left $ "Couldn't parse: "++s

parseProg : List String -> Either String (List Instruction)
parseProg xs = sequence $ map parseLine xs

-- Evaluator
evalCheckOp : Check -> (Int -> Int -> Bool)
evalCheckOp LT  = (<)
evalCheckOp GT  = (>)
evalCheckOp LTE = (<=)
evalCheckOp GTE = (>=)
evalCheckOp EQ  = (==)
evalCheckOp NEQ = (/=)

evalOp : Operation -> Int -> Int -> Int
evalOp INC = (+)
evalOp DEC = (-)

lookupRef : String -> State (List (String, Int)) Int
lookupRef s = gets ((fromMaybe 0) . (List.lookup s))

setRef : String -> Int -> State (List (String, Int)) ()
setRef ref newVal =  do prev <- lookupRef ref
                        modify (setRef' ref prev newVal)
  where
    setRef' : String -> Int -> Int -> List (String, Int) -> List (String, Int)
    setRef' ref prev newVal xs = (ref, newVal) :: (delete (ref, prev) xs)

evalInstr : Instruction -> State (List (String, Int)) ()
evalInstr (MkInstruction target op opVal checkRef checkOp checkVal) = do
  c <- lookupRef checkRef
  if evalCheckOp checkOp c checkVal
  then do v <- lookupRef target
          setRef target (evalOp op v opVal)
  else pure ()

-- Solution
-- Part 1
evalProg : List Instruction -> (List (String, Int))
evalProg is = execState (sequence (map evalInstr is)) [("", 0)]

computeMax : List Int -> Int
computeMax xs = fromMaybe 0 $ last' $ sort xs

export
part1 : Either String Int
part1 = case parseProg input of
             (Left error) => (Left error)
             (Right prog) => Right $ computeMax $ map snd $ evalProg prog

-- Part 2
traceEval : List a -> State a b -> List a
traceEval [] run = []
traceEval log@(state :: xs) run = (execState run state) :: log

traceProg : List Instruction -> List (List (String, Int))
traceProg is = foldl traceEval [[("", 0)]] (map evalInstr is)

export
part2 : Either String Int
part2 = case parseProg input of
             (Left error) => (Left error)
             (Right prog) => Right $ computeMax $ map snd $ concat $ traceProg prog
