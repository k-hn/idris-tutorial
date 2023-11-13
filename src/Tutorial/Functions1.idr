module Tutorial.Functions1


-- Functions with more than one Argument
isTriple : Integer -> Integer -> Integer -> Bool
isTriple x y z = x * x + y * y == z * z


-- Function Composition
square : Integer -> Integer
square n = n * n

times2 : Integer -> Integer
times2 n = n * 2

squareTimes2 : Integer -> Integer
squareTimes2 = times2 . square

dotChain : Integer -> String
dotChain = reverse . show . square . square . times2 . times2


-- Higer-order Functions
isEven : Integer -> Bool
isEven n = mod n 2 == 0

testSquare : (Integer -> Bool) -> Integer -> Bool
testSquare fun n = fun (square n)

twice : (Integer -> Integer) -> Integer -> Integer
twice fun n = fun (fun n)


-- Currying
partialExample : Integer -> Bool
partialExample = isTriple 3 4


-- Anonymous Functions
someTest : Integer -> Bool
someTest n = n >= 3 || n <= 10


-- Operators
infixr 4 >>>

(>>>) : (Bits8 -> Bits8) -> (Bits8 -> Bits8) -> Bits8 -> Bits8
f1 >>> f2 = f2 . f1

foo : Bits8 -> Bits8
foo n = 2 * n + 3

test : Bits8 -> Bits8
test = foo >>> foo >>> foo >>> foo


-- Operator Sections
applyToLen : (Integer -> Integer) -> Integer
applyToLen f = f 10


-- Infix Notation for Non-Operators
infixl 8 `plus`
infixl 9 `mult`

plus : Integer -> Integer -> Integer
plus = (+)

mult : Integer -> Integer -> Integer
mult = (*)

arithTest : Integer
arithTest = 5 `plus` 10 `mult` 12

arithTest' : Integer
arithTest' = 5 + 10 * 12
