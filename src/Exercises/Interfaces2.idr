module Exercises.Interfaces2

-- 1
interface Comp a where
  comp : a -> a -> Ordering

interface Equals a where
  eq : a -> a -> Bool

  neq : a -> a -> Bool
  neq a1 a2 = not (eq a1 a2)

interface Concat a where
  concatenate : a -> a -> a

interface Concat a => Empty a where
  empty : a

-- Implementations for pairs
--- Comp
implementation Comp String where
  comp = compare

implementation Comp Integer where
  comp = compare

implementation Comp Double where
  comp = compare

implementation Comp a => Comp b => Comp (a, b) where
  comp (x1, y1) (x2, y2) = 
    case comp x1 x2 of
      EQ => comp y1 y2
      result => result 

-- Equals
implementation Equals Integer where
  eq = (==)

implementation Equals Double where
  eq = (==)

implementation Equals String where
  eq = (==)

implementation Equals a => Equals b => Equals (a, b) where
  eq (x1, y1) (x2, y2) = eq x1 x2 && eq y1 y2


-- Concat
implementation Concat String where
  concatenate = (++)

implementation Concat (List a) where
  concatenate = (++)

implementation Concat a => Concat b => Concat (a, b) where
  concatenate (x1, y1) (x2, y2) = (concatenate x1 x2, concatenate y1 y2)

-- Empty
implementation Empty String where
  empty = ""

implementation Empty (List a) where
  empty = []

implementation Empty a => Empty b => Empty (a, b) where
  empty = (empty, empty)


-- 2
data Tree : Type -> Type where
  Leaf : a -> Tree a
  Node : Tree a -> Tree a -> Tree a


implementation Equals a => Equals (Tree a) where
  eq (Leaf x) (Leaf y) = eq x y
  eq (Node l1 r1) (Node l2 r2) = eq l1 l2 && eq r1 r2
  eq _ _ = False

implementation Concat (Tree a) where
  concatenate = Node
