module Exercises.Functions1

-- 1
-- return the square of integer n
square : Integer -> Integer
square n = n * n

-- return true, if the square of the provided integer passes the test `fun`
testSquare : (Integer -> Bool) -> Integer -> Bool
testSquare fun = fun . square

-- run function `fun` twice on the provided integer
twice : (Integer -> Integer) -> Integer -> Integer
twice fun = fun . fun


-- 2
-- return true if integer is even, else false
isEven : Integer -> Bool
isEven n = mod n 2 == 0

-- return true if integer is odd, else false
isOdd : Integer -> Bool
isOdd = not . isEven


-- 3
-- return true, if integer x is the square of integer y
isSquareOf : Integer -> Integer -> Bool
isSquareOf x y = x == square y


-- 4
-- return true, if integer is less than or equal to 100, else false
isSmall : Integer -> Bool
isSmall x = x <= 100


-- 5
-- return true, if the absolute value of the integer is less than or equal to 100, else false
absIsSmall : Integer -> Bool
absIsSmall x = (abs x) <= 100


-- 6
-- return true, if and only if both predicates hold
and : (Integer -> Bool) -> (Integer -> Bool) -> Integer -> Bool
and f g x = (f x) && (g x)

-- return true, if and only if at least one predicate holds
or : (Integer -> Bool) -> (Integer -> Bool) -> Integer -> Bool
or f g x = (f x) || (g x)

-- return true, if the predicate does not hold
negate : (Integer -> Bool) -> Integer -> Bool
negate f x = (not . f) x


-- 7
-- return true, if and only if both predicates hold
(&&) : (Integer -> Bool) -> (Integer -> Bool) -> Integer -> Bool
x && y = and x y

-- return true, if and only if at least one predicate holds
(||) : (Integer -> Bool) -> (Integer -> Bool) -> Integer -> Bool
x || y = or x y

-- return true, if the predicate does not hold
not : (Integer -> Bool) -> Integer -> Bool
not f x = negate f x
