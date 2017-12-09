module Day7
import FileProvider
import Data.Vect

%default total
%language TypeProviders

%provide (inputString : String) with readString "Day7_short.txt"

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

data Tree : (weightSum : Nat) -> Type where
  Leaf : (name : String) -> (weight : Nat) -> (Tree weight)
  Node : (name : String) -> (weight : Nat) -> (ds : Vect n (Tree subWeight)) -> (Tree (weight + (n * subWeight)))

-- Accessors
getName : Tree w -> String
getName (Leaf name w) = name
getName (Node name weight ds) = name

getWeightSum : Tree w -> Nat
getWeightSum {w} x = w

getWeight : Tree w -> Nat
getWeight (Leaf name w) = w
getWeight (Node name weight ds) = weight

getInfo : Tree w -> (String, Nat, Nat)
getInfo t = (getName t, getWeight t, getWeightSum t)

-- Dependent Accessor
infoPaired : (n ** Tree n) -> (String, Nat, Nat)
infoPaired (x ** pf) = getInfo pf

-- Tree Builder
childrenTrees : Vect n String -> List (m ** Tree m) -> Maybe (Vect n (l ** Tree l))
childrenTrees xs ys = sequence $ map (\x => find (matchingName x) ys) xs
  where matchingName : String -> (n ** Tree n) -> Bool
        matchingName s (x ** pf) = s == getName pf

maybeMakeTree : (Vect n (l ** Tree l)) -> Either (List Nat) (m ** Vect n (Tree m))
maybeMakeTree [] = Right (0 ** [])
maybeMakeTree ((x ** xt) :: xs) =
  case maybeMakeTree xs of
    (Right (_ ** [])) => Right (x ** [xt])
    (Right (y ** yt)) => case decEq y x of
                            (Yes Refl) => Right (y ** (xt :: yt))
                            (No contra) => Left $ x :: map (const y) (toList xs)
    (Left is) => Left (x :: is)

makeTree : String -> (weight : Nat) -> (w ** Vect n (Tree w)) -> (x ** (Tree x))
makeTree name weight (w ** sts) = (_ ** (Node name weight sts))

partial
iter : List (String, Nat, (n ** Vect n String)) -> List (m ** Tree m) -> Either (List (String, Nat, Nat)) (List (l ** Tree l))
iter [] ys = Right ys
iter ((name, weight, (n ** children)) :: xs) ys =
  if n == 0
  then iter xs ((weight ** (Leaf name weight)) :: ys)
  else case childrenTrees children ys of
            Nothing => iter (xs ++ [(name, weight, (n ** children))]) ys
            (Just cs) => case maybeMakeTree cs of
                              (Left l) => Left $ map infoPaired (toList cs)
                              (Right r) => iter xs ((makeTree name weight r) :: ys)

-- Solution
partial
part2 : Either (List (String, Nat,Nat)) (List (l ** Tree l))
part2 = iter input []
