module Exercises.DataTypes3

-- 1
data UnitOfTime = Second
                | Minute
                | Hour
                | Day
                | Week

record TimeSpan where
  constructor MkTimeSpan
  unit : UnitOfTime
  time_length : Integer

-- calculate the number of seconds from a
-- number of steps in the given unit of time
total
toSeconds : UnitOfTime -> Integer -> Integer
toSeconds Second x = x
toSeconds Minute x = x * 60
toSeconds Hour x = x * 60 * 60
toSeconds Day x = x * 60 * 60 * 24
toSeconds Week x = x * 60 * 60 * 24 * 7

convertTimeSpanToSeconds : TimeSpan -> Integer
convertTimeSpanToSeconds (MkTimeSpan unit time_length) = toSeconds unit time_length


-- 2
eqTimeSpan : TimeSpan -> TimeSpan -> Bool
eqTimeSpan ts1 ts2 = (convertTimeSpanToSeconds ts1) == (convertTimeSpanToSeconds ts2)


-- 3
-- returns a string representation of the time span unit
showTimeSpanUnit : TimeSpan -> String
showTimeSpanUnit (MkTimeSpan Second _) = "second(s)"
showTimeSpanUnit (MkTimeSpan Minute _) = "minute(s)"
showTimeSpanUnit (MkTimeSpan Hour _) = "hour(s)"
showTimeSpanUnit (MkTimeSpan Week _) = "week(s)"
showTimeSpanUnit (MkTimeSpan Day _) = "day(s)"

-- returns a string representation of the time span in seconds
showTimeSpanSeconds : TimeSpan -> String
showTimeSpanSeconds ts = show (convertTimeSpanToSeconds ts) ++ " second(s)"

-- returns a string representation of the time span
showTimeSpan : TimeSpan -> String
showTimeSpan ts@(MkTimeSpan _ time_length) = 
  (show time_length) ++ " " ++ showTimeSpanUnit ts


prettyPrintTimeSpan : TimeSpan -> String
prettyPrintTimeSpan (MkTimeSpan Second time_length) = show time_length ++ "second(s)"
prettyPrintTimeSpan ts = showTimeSpan ts ++ ": " ++ showTimeSpanSeconds ts


-- 3
total
convertToSecondsTimeSpan : TimeSpan -> TimeSpan
convertToSecondsTimeSpan ts = MkTimeSpan Second (convertTimeSpanToSeconds ts)

total
compareUnitOfTime : UnitOfTime -> UnitOfTime -> UnitOfTime
compareUnitOfTime Second _ = Second
compareUnitOfTime _ Second = Second
compareUnitOfTime Minute _ = Minute
compareUnitOfTime _ Minute = Minute
compareUnitOfTime Hour _ = Hour
compareUnitOfTime _ Hour = Hour
compareUnitOfTime Day _ = Day
compareUnitOfTime _ Day = Day
compareUnitOfTime _ _ = Week

total
fromSeconds : UnitOfTime -> Integer -> Integer
fromSeconds Second x = x
fromSeconds Minute x = x `div` 60
fromSeconds Hour x = x `div` (60 * 60)
fromSeconds Day x = x `div` (60 * 60 * 24)
fromSeconds Week x = x `div` (60 * 60 * 24 * 7) 

total  
convertTimeSpanFromSeconds : TimeSpan -> UnitOfTime -> TimeSpan
convertTimeSpanFromSeconds ts unit = 
  MkTimeSpan unit (fromSeconds unit ts.time_length)

total
getSmallerUnitOfTime : TimeSpan -> TimeSpan -> UnitOfTime
getSmallerUnitOfTime ts1 ts2 = compareUnitOfTime ts1.unit ts2.unit

addSecondsTimeSpans : TimeSpan -> TimeSpan -> TimeSpan
addSecondsTimeSpans (MkTimeSpan Second time_length1) (MkTimeSpan Second time_length2) = 
  MkTimeSpan Second (time_length1 + time_length2)
addSecondsTimeSpans ts1 ts2 = 
  addSecondsTimeSpans (convertToSecondsTimeSpan ts1) (convertToSecondsTimeSpan ts2)

addTimeSpans : TimeSpan -> TimeSpan -> TimeSpan
addTimeSpans ts1 ts2 =
  convertTimeSpanFromSeconds
    (addSecondsTimeSpans ts1 ts2)
    (getSmallerUnitOfTime ts1 ts2)
    
