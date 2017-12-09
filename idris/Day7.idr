module Day7
import FileProvider
import Data.Vect

%default total
%language TypeProviders

%provide (inputString : String) with readString "Day7_short.txt"

data Tree : (weightSum : Nat) -> Type where
  Leaf : (name : String) -> (weight : Nat) -> (Tree weight)

  Node : (name : String)
      -> (weight : Nat)
      -> (ds : Vect n (Tree subWeight))
      -> (Tree (weight + (n * subWeight)))

-- Accessors
getName : Tree w -> String
getName (Leaf name _) = name
getName (Node name _ _) = name

getWeightSum : Tree w -> Nat
getWeightSum {w} _ = w

getWeight : Tree w -> Nat
getWeight (Leaf _ weight) = weight
getWeight (Node _ weight _) = weight

getInfo : Tree w -> (String, Nat, Nat)
getInfo t = (getName t, getWeight t, getWeightSum t)

-- Dependent Accessor
infoPaired : (n ** Tree n) -> (String, Nat, Nat)
infoPaired (_ ** pf) = getInfo pf

-- Parsers & Input
parseLine : String -> Maybe (String, Nat, (n ** Vect n String))
parseLine s =
  let (name, s1) = break (== ' ') s in
  let (_, s2) = span (flip List.elem [' ', '(']) s1 in
  let (weight, s3) = break (== ')') s2 in
  let (_, s4) = span (flip List.elem [')',' ','-','>']) s3 in
  let children = (map trim $ split (== ',') s4) \\ [""] in
  Just (name, (cast weight), (length children ** fromList children))

input : List (String, Nat, (n ** Vect n String))
input = catMaybes $ map parseLine $ lines inputString

-- Tree Builder
findChildren : Vect n String
             -> List (m ** Tree m)
             -> Maybe (Vect n (l ** Tree l))
findChildren xs ys = sequence $ map (\x => find (matchingName x) ys) xs
  where matchingName : String -> (n ** Tree n) -> Bool
        matchingName s (x ** pf) = s == getName pf

unifyChildren : (Vect n (l ** Tree l)) -> Maybe (m ** Vect n (Tree m))
unifyChildren [] = Just (0 ** [])
unifyChildren ((x ** xt) :: xs) =
  case unifyChildren xs of
    (Just (_ ** [])) => Just (x ** [xt])
    (Just (y ** yt)) => case decEq y x of
                            (Yes Refl) => Just (y ** (xt :: yt))
                            (No contra) => Nothing
    Nothing => Nothing

makeNode : (name     : String)
        -> (weight   : Nat)
        -> (children : (w ** Vect n (Tree w)))
        -> (x ** (Tree x))
makeNode name weight (w ** sts) = (_ ** (Node name weight sts))

partial
depsOrder : (xs  : List (String, Nat, (n ** Vect n String)))
         -> (tmp : List (String, Nat, (n ** Vect n String)))
         -> List (String, Nat, (n ** Vect n String))
depsOrder [] deps = reverse deps
depsOrder ((name, weight, (n ** children)) :: xs) deps =
  if n == 0 || all (\c => elem c (map fst deps)) children
  then depsOrder xs ((name, weight, (n ** children)) :: deps)
  else depsOrder (xs ++ [(name, weight, (n ** children))]) deps

makeTree : (inputs   : List (String, Nat, (n ** Vect n String)))
        -> (tmpTrees : List (m ** Tree m))
        -> Either (String, (List (String, Nat, Nat)))
                  (l ** Tree l)
makeTree [] [] = Left ("Can't build tree from empty list", [])
makeTree [] (tree :: others) = Right tree
makeTree ((name, weight, (n ** children)) :: xs) ts =
  if n == 0
  then makeTree xs ((weight ** (Leaf name weight)) :: ts)
  else case findChildren children ts of
            Nothing => Left ("Not in deps order", [])
            (Just cs) => case unifyChildren cs of
                              Nothing => Left ("Not balanced", map infoPaired (toList cs))
                              (Just r) => makeTree xs ((makeNode name weight r) :: ts)
partial
part1 : Maybe String
part1 = fst <$> (last' $ depsOrder input [])

-- Solution
partial
part2 : Either (String, (List (String, Nat,Nat))) (l ** Tree l)
part2 = makeTree (depsOrder input []) []
