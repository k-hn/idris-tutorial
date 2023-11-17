module Exercises.Interfaces1

-- 1
interface Comp a where
  comp : a -> a -> Ordering

implementation Comp Integer where
  comp = compare

implementation Comp Double where
  comp = compare

implementation Comp String where
  comp = compare

anyLarger : Comp a => List a -> a -> Bool
anyLarger Nil _ = False
anyLarger (x :: xs) ref =
  case comp x ref of
    GT =>  True
    _ => anyLarger xs ref


-- 2
allLarger : Comp a => List a -> a -> Bool
allLarger Nil _ = True
allLarger (x :: xs) ref = 
  case comp x ref of
    GT => True && allLarger xs ref
    _ => False


-- 3
maxElemHelper : Comp a => List a -> a -> a
maxElemHelper Nil acc = acc
maxElemHelper (x :: xs) acc = 
  case comp x acc of
    GT => maxElemHelper xs x
    _ => maxElemHelper xs acc

maxElem : Comp a => List a -> Maybe a
maxElem Nil = Nothing
maxElem (x :: xs) = Just (maxElemHelper xs x)

-- 4
interface Concat a where
  concatenate : a -> a -> a

implementation Concat (List a) where
  concatenate = (++)

implementation Concat String where
  concatenate = (++)


-- 5
concatListHelper : Concat a => List a -> a -> a
concatListHelper Nil acc = acc
concatListHelper (x :: xs) acc = concatListHelper xs (concatenate acc x)

concatList : Concat a => List a -> Maybe a
concatList Nil = Nothing
concatList (x :: xs) = Just (concatListHelper xs x)
