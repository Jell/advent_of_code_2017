module Day7
import FileProvider
import Data.Vect

%default total
%language TypeProviders

%provide (inputString : String) with readString "Day7_short.txt"

data Tree : (weightSum : Nat) -> Type where
  Leaf : (name : String) -> (weight : Nat) -> (Tree weight)
  Node : (name : String) -> (weight : Nat) -> (ds : Vect n (Tree subWeight)) -> (Tree (weight + (n * subWeight)))

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

parseLine : String -> Maybe (String, Nat, (n ** Vect n String))
parseLine s =
  let (name, s') = break (== ' ') s in
  let (_, s'') = span (flip List.elem [' ', '(']) s' in
  let (weight, s''') = break (== ')') s'' in
  let (_, s'''') = span (flip List.elem [')',' ','-','>']) s''' in
  let children = (map trim $ split (== ',') s'''') \\ [""] in
  Just (name, (cast weight), (length children ** fromList children))

input : List (String, Nat, (n ** Vect n String))
input = catMaybes $ map parseLine $ lines inputString

nameFromPaired : (n ** Tree n) -> String
nameFromPaired (x ** pf) = getName pf

weightFromPaired : (n ** Tree n) -> Nat
weightFromPaired (x ** pf) = getWeight pf

matchingName : String -> (n ** Tree n) -> Bool
matchingName s (x ** pf) = s == getName pf

childrenTrees : Vect n String -> List (m ** Tree m) -> Maybe (Vect n (l ** Tree l))
childrenTrees xs ys = sequence $ map (\x => find (matchingName x) ys) xs

maybeMakeTree : (Vect n (l ** Tree l)) -> Either (List Nat) (m ** Vect n (Tree m))
maybeMakeTree [] = Right (0 ** [])
maybeMakeTree ((x ** xt) :: xs) =
  case maybeMakeTree xs of
    (Right (_ ** [])) => Right (x ** [xt])
    (Right (y ** yt)) => case decEq y x of
                            (Yes Refl) => Right (y ** (xt :: yt))
                            (No contra) => Left $ x :: map (const y) (toList xs)
    (Left is) => Left (x :: is)

makeTree : String -> (weight : Nat) -> (w ** Vect n (Tree w)) -> (w ** (Tree (weight + (n * w))))
makeTree name weight (w ** sts) = (w ** (Node name weight sts))

treeIsTree : (weight : Nat) -> (n : Nat) -> (w : Nat ** (Tree (weight + (n * w)))) -> (x ** (Tree x))
treeIsTree weight n (w ** t) = (_ ** t)

wwspaired : (n ** Tree n) -> (String, Nat, Nat)
wwspaired (x ** pf) = getInfo pf

partial
iter : List (String, Nat, (n ** Vect n String)) -> List (m ** Tree m) -> Either (List (String, Nat, Nat)) (List (l ** Tree l))
iter [] ys = Right ys
iter ((name, weight, (n ** children)) :: xs) ys =
  if n == 0
  then iter xs ((weight ** (Leaf name weight)) :: ys)
  else case childrenTrees children ys of
            Nothing => iter (xs ++ [(name, weight, (n ** children))]) ys
            (Just cs) => case maybeMakeTree cs of
                              (Left l) => Left $ map wwspaired (toList cs)
                              (Right r) => iter xs ((treeIsTree weight n $ makeTree name weight r) :: ys)

partial
part1 : Either (List (String, Nat,Nat)) (List (l ** Tree l))
part1 = iter input []
