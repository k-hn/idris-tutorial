module Tutorial.Interfaces

%default total

namespace Bool
  export
  size : Bool -> Integer
  size True = 1
  size False = 0

namespace Integer
  export
  size : Integer -> Integer
  size = id

namespace List
  export
  size : List a -> Integer
  size = cast . length

-- Idris can disambiguate `size` used in `mean` so it knows to use List.size instead of the 
-- other options
mean : List Integer -> Integer
mean xs = sum xs `div` size xs


-- Interface Basics
-- Sometimes even overloading could lead to a lot of code duplication
cmp : String -> String -> Ordering

lessThan' : String -> String -> Bool
lessThan' s1 s2 = LT == cmp s1 s2

greaterThan' : String -> String -> Bool
greaterThan' s1 s2 = GT == cmp s1 s2

minimum' : String -> String -> String
minimum' s1 s2 = 
  case cmp s1 s2 of
    LT => s1
    _ => s2

maximum' : String -> String -> String
maximum' s1 s2 =
  case cmp s1 s2 of
    GT => s1
    _ => s2

-- A way of having to avoid defining overloads for all types would be to 
-- have the caller pass in the comparison function and paraterise 
-- the types of the function
minimumBy : (a -> a -> Ordering) -> a -> a -> a
minimumBy f a1 a2 =
  case f a1 a2 of
    LT => a1
    _ => a2

-- A better option however would be to use interfaces
interface Comp a where
  comp : a -> a -> Ordering

implementation Comp Bits8 where
  comp = compare

implementation Comp Bits16 where
  comp = compare

lessThan : Comp a => a -> a -> Bool
lessThan s1 s2 = LT == comp s1 s2

greaterThan : Comp a => a -> a -> Bool
greaterThan s1 s2 = GT == comp s1 s2

minimum : Comp a => a -> a -> a
minimum s1 s2 =
  case comp s1 s2 of
    LT => s1
    _ => s2

maximum : Comp a => a -> a -> a
maximum s1 s2 =
  case comp s1 s2 of
    GT => s1
    _ => s2
