module Tutorial.DataTypes

-- Datatypes consisting a finite set of values
-- Enumerations
data Weekday = Monday
              | Tuesday
              | Wednesday
              | Thursday
              | Friday
              | Saturday
              | Sunday


-- Pattern Matching
total
next : Weekday -> Weekday
next Monday = Tuesday
next Tuesday = Wednesday
next Wednesday = Thursday
next Thursday = Friday
next Friday = Saturday
next Saturday = Sunday
next Sunday = Monday


-- Catch-all Patterns
total
isWeekend : Weekday -> Bool
isWeekend Saturday = True
isWeekend Sunday = True
isWeekend _ = False

total
eqWeekday : Weekday -> Weekday -> Bool
eqWeekday Monday Monday = True
eqWeekday Tuesday Tuesday = True
eqWeekday Wednesday Wednesday = True
eqWeekday Thursday Thursday = True
eqWeekday Friday Friday = True
eqWeekday Saturday Saturday = True
eqWeekday Sunday Sunday = True
eqWeekday _ _ = False


-- Enumeration Types in the Prelude
total
negate : Bool -> Bool
negate False = True
negate True = False

total
compareBool : Bool -> Bool -> Ordering
compareBool False False = EQ
compareBool False True = LT
compareBool True True = EQ
compareBool True False = GT


-- Case Expressions
-- returns the larger of the two arguments
total
maxBits8 : Bits8 -> Bits8 -> Bits8
maxBits8 x y = 
  case compare x y of
    LT => y
    _ => x


-- If Then Else
total
maxBits8' : Bits8 -> Bits8 -> Bits8
maxBits8' x y = if compare x y == LT then y else x


-- Naming Conventions: Identifiers
-- data foo = bar | baz  -- invalid
data Foo = Bar | Baz     -- valid

foo : Bits32 -> Bits32
foo = (* 2)

Baa : Bits32 -> Bits32
Baa = foo


-- Sum Types
data Title = Mr | Mrs | Other String

total
dr : Title
dr = Other "Dr."

total
showTitle : Title -> String
showTitle Mr = "Mr."
showTitle Mrs = "Mrs."
showTitle (Other x) = x

total
greet : Title -> String -> String
greet t name = "Hello, " ++ showTitle t ++ " " ++ name ++ "!"

-- encapsulating two different login types in data type
data Credentials = Password String Bits64 | Key String String

total
login : Credentials -> String
login (Password "Anderson" 6665443) = greet Mr "Anderson"
login (Key "Y" "xyz") = greet (Other "Agent") "Y"
login _ = "Access denied"


-- Records
-- Records a a product type - A composite representation of an entity

record User where
  constructor MkUser
  name : String
  title : Title
  age : Bits8

total
agentY : User
agentY = MkUser "Y" (Other "Agent") 51

total
drNo : User
drNo = MkUser "No" dr 73

total
greetUser : User -> String
greetUser (MkUser name title _) = greet title name

failing "Mismatch between: String and Title"
  greetUser' : User -> String
  greetUser' (MkUser n t _) = greet n t


-- Syntactic Sugar for Records

total 
incAge : User -> User
incAge (MkUser name title age) = MkUser name title (age + 1)

total
incAge2 : User -> User
incAge2 u = {age := u.age + 1} u

total
incAge3 : User -> User
incAge3 u = { age $= (+ 1) } u

total
incAge4 : User -> User
incAge4 u = { age $= \x => x + 1 } u

total
incAge5 : User -> User
incAge5 = { age $= (+ 1) }

total
drNoJunior : User
drNoJunior = { name $= (++ " Jr."), title := Mr, age := 17 } drNo


-- Tuples
record Foo2 where
  constructor MkFoo2
  wd : Weekday
  bool : Bool

-- Total possible value types for foo: 7 * 2 = 14
-- Possible values for Weekday: 7
-- Possible values for Bool: 2

-- Pair is the canonical product type pair
total
weekdayAndBool : Weekday -> Bool -> Pair Weekday Bool
weekdayAndBool wd b = MkPair wd b

total
weekdayAndBool2 : Weekday -> Bool -> (Weekday, Bool)
weekdayAndBool2 wd b = (wd, b)

total
triple : Pair Bool (Pair Weekday String)
triple = MkPair False (Friday, "foo")

total
triple2 : (Bool, Weekday, String)
triple2 = (False, Friday, "foo")

total
bar : Bool
bar = case triple of
  (b, wd, _) => b && isWeekend wd



-- As Patterns
total
baz : (Bool, Weekday, String) -> (Nat, Bool, Weekday, String)
baz t@(_, _, s) = (length s, t)


-- Generic Data Types
--- Maybe

data MaybeWeekday = WD Weekday | NoWeekday

total
readWeekday : String -> MaybeWeekday
readWeekday "Monday" = WD Monday
readWeekday "Tuesday" = WD Tuesday
readWeekday "Wednesday" = WD Wednesday
readWeekday "Thursday" = WD Thursday
readWeekday "Friday" = WD Friday
readWeekday "Saturday" = WD Saturday
readWeekday "Sunday" = WD Sunday
readWeekday _ = NoWeekday

-- In Idris, we can parameterize types
-- Option a -> Type constructor
-- Some a, None -> Data Constructors
data Option a = Some a | None

total
readBool : String -> Option Bool
readBool "True" = Some True
readBool "False" = Some False
readBool _ = None

total
safeDiv : Integer -> Integer -> Option Integer
safeDiv n 0 = None
safeDiv n k = Some (n `div` k)

total
safeDiv' : Integer -> Integer -> Maybe Integer
safeDiv' n 0 = Nothing
safeDiv' n k = Just (n `div` k)


--- Either
data Validated e a = Invalid e | Valid a

total
readWeekdayV : String -> Validated String Weekday
readWeekdayV "Monday" = Valid Monday
readWeekdayV "Tuesday" = Valid Tuesday
readWeekdayV "Wednesday" = Valid Wednesday
readWeekdayV "Thursday" = Valid Thursday
readWeekdayV "Friday" = Valid Friday
readWeekdayV "Saturday" = Valid Saturday
readWeekdayV "Sunday" = Valid Sunday
readWeekdayV s = Invalid ("Not a weekday: " ++ s)


--- List
-- The singly-linked list
-- sample implementation
data Seq a = Nil | (::) a (Seq a)

total
ints : List Int64
ints = 1 :: 2 :: -3 :: Nil

total
ints2 : List Int64
ints2 = [1, 2, -3]

total
ints3 : List Int64
ints3 = []

total
intSum : List Integer -> Integer
intSum Nil = 0
intSum (n :: ns) = n + intSum ns


-- Generic Functions
total
integerFromOption : Integer -> Option Integer -> Integer
integerFromOption _ (Some y) = y
integerFromOption x None = x

-- generalising the above by parameterising the function over types
total
fromOption : a -> Option a -> a
fromOption x None = x
fromOption _ (Some y) = y

total
option : b -> (a -> b) -> Option a -> b
option _ f (Some y) = f y
option x _ None = x

total
handleBool : Option Bool -> String
handleBool = option "Not a boolean value." show 


-- Alternative Syntax for Data Definitions
namespace GADT
  data Option : Type -> Type where
    Some : a -> Option a
    None : Option a

  data Validated : Type -> Type -> Type where
    Invalid : e -> Validated e a
    Valid : a -> Validated e a

  data Seq : Type -> Type where
    Nil : Seq a
    (::) : a -> GADT.Seq a -> Seq a
