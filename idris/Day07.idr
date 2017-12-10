module Day07
import FileProvider
import Data.Vect

%default total
%language TypeProviders

%provide (inputString : String) with readString "Day07_short.txt"

export
data Tower : (weightSum : Nat) -> Type where
  NoDisk : (name : String) -> (weight : Nat) -> (Tower weight)

  Disk : (name : String)
      -> (weight : Nat)
      -> (ds : Vect (S n) (Tower subWeight))
      -> (Tower (weight + ((S n) * subWeight)))

-- Accessors
getName : Tower w -> String
getName (NoDisk name _) = name
getName (Disk name _ _) = name

getWeightSum : Tower w -> Nat
getWeightSum {w} _ = w

getWeight : Tower w -> Nat
getWeight (NoDisk _ weight) = weight
getWeight (Disk _ weight _) = weight

getInfo : Tower w -> (String, Nat, Nat)
getInfo t = (getName t, getWeight t, getWeightSum t)

-- Dependent Accessor
infoPaired : (n ** Tower n) -> (String, Nat, Nat)
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

-- Dependency solver
depsOrder : (xs  : List (String, Nat, (n ** Vect n String)))
         -> (tmp : List (String, Nat, (n ** Vect n String)))
         -- Add bound retries for totality check
         -> {default (fact (length xs)) retries : Nat}
         -> Maybe (List (String, Nat, (n ** Vect n String)))
depsOrder [] deps = Just $ reverse deps
depsOrder {retries=(S rets)} ((name, weight, (n ** children)) :: xs) deps =
  if n == 0 || all (\c => elem c (map fst deps)) children
  then depsOrder {retries=rets} xs ((name, weight, (n ** children)) :: deps)
  else depsOrder {retries=rets} (xs ++ [(name, weight, (n ** children))]) deps
depsOrder _ _ = Nothing

-- Tower Builder
findChildren : Vect n String
             -> List (m ** Tower m)
             -> Maybe (Vect n (l ** Tower l))
findChildren xs ys = sequence $ map (\x => find (matchingName x) ys) xs
  where matchingName : String -> (n ** Tower n) -> Bool
        matchingName s (x ** pf) = s == getName pf

unifyChildren : (Vect n (l ** Tower l)) -> Maybe (m ** Vect n (Tower m))
unifyChildren [] = Just (0 ** [])
unifyChildren ((x ** xt) :: xs) =
  case unifyChildren xs of
    (Just (_ ** [])) => Just (x ** [xt])
    (Just (y ** yt)) => case decEq y x of
                            (Yes Refl) => Just (y ** (xt :: yt))
                            (No contra) => Nothing
    Nothing => Nothing

makeDisk : (name     : String)
        -> (weight   : Nat)
        -> (children : (w ** Vect (S n) (Tower w)))
        -> (x ** (Tower x))
makeDisk name weight (w ** sts) = (_ ** (Disk name weight sts))

makeTower : (inputs   : List (String, Nat, (n ** Vect n String)))
        -> (tmpTowers : List (m ** Tower m))
        -> Either (String, (List (String, Nat, Nat)))
                  (l ** Tower l)
makeTower [] [] = Left ("Can't build tree from empty list", [])
makeTower [] (tree :: others) = Right tree
makeTower ((name, weight, (n ** children)) :: xs) ts =
  case n of
    Z => makeTower xs ((weight ** (NoDisk name weight)) :: ts)
    (S k) => case findChildren children ts of
                  Nothing => Left ("Not in deps order", [])
                  (Just cs) => case unifyChildren cs of
                                    Nothing => Left ("Not balanced", map infoPaired (toList cs))
                                    (Just r) => makeTower xs ((makeDisk name weight r) :: ts)

-- Solution
export
part1 : Maybe String
part1 = fst <$> (depsOrder input [] >>= last')

export
part2 : Either (String, (List (String, Nat,Nat))) (l ** Tower l)
part2 = case depsOrder input [] of
             Just deps => makeTower deps []
             Nothing => Left ("Failed to build deps", [])

-- So that we can print the result
namespace TowerShow
  partial
  show : (w ** Tower w) -> String
  show (_ ** NoDisk name weight) = name ++ " (" ++ (show weight) ++ ")"
  show (_ ** Disk {subWeight} {n} name weight cs) =
       foldl (++) (diskStr ++ "\n") (map showChild cs)
    where sumWeight : Nat
          sumWeight = weight + (n * subWeight)
          diskStr = name ++ " -- " ++ (show sumWeight) ++ " (" ++ (show weight) ++ ")"
          showChild c = unlines $ map ("    " ++) $ lines $ show (_ ** c)

namespace ResultShow
  partial
  show : Either (String, (List (String, Nat,Nat))) (w ** Tower w) -> String
  show (Left l) = "Failed: " ++ (show l)
  show (Right t) = show t

export partial
part2str : String
part2str = show part2
