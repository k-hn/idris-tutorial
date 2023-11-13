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
