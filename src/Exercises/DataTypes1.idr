module Exercises.DataTypes1

-- 1
and : Bool -> Bool -> Bool
and True True = True
and _ _ = False

or : Bool -> Bool -> Bool
or False False = False
or _ _ = True


-- 2
data UnitOfTime = Second
                | Minute
                | Hour
                | Day
                | Week

-- calculate the number of seconds from a
-- number of steps in the given unit of time
total
toSeconds : UnitOfTime -> Integer -> Integer
toSeconds Second x = x
toSeconds Minute x = x * 60
toSeconds Hour x = x * 60 * 60
toSeconds Day x = x * 60 * 60 * 24
toSeconds Week x = x * 60 * 60 * 24 * 7

-- Given a number of seconds, calculate the
-- number of steps in the given unit of time
total
fromSeconds : UnitOfTime -> Integer -> Integer
fromSeconds Second x = x
fromSeconds Minute x = x `div` 60
fromSeconds Hour x = x `div` (60 * 60)
fromSeconds Day x = x `div` (60 * 60 * 24)
fromSeconds Week x = x `div` (60 * 60 * 24 * 7) 

-- convert the number of steps in a given unit of time
-- to the number of steps in another unit of time.
-- use `fromSeconds` and `toSeconds` in your implementation
total
convert : UnitOfTime -> Integer -> UnitOfTime -> Integer
convert from_time_unit x to_time_unit = fromSeconds to_time_unit (toSeconds from_time_unit x)


-- 3
data Element = H | C | N | O | F

atomicMass : Element -> Double
atomicMass H = 1.008
atomicMass C = 12.011
atomicMass N = 14.007
atomicMass O = 15.999
atomicMass F = 18.9984
