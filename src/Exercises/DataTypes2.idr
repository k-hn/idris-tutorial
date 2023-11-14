module Exercises.DataTypes2

-- 1
data Title = Mr | Mrs | Other String

total
showTitle : Title -> String
showTitle Mr = "Mr."
showTitle Mrs = "Mrs."
showTitle (Other x) = x

-- equality test for Title, returns true if title_1 and title_2 match
total
eqTitle : Title -> Title -> Bool
eqTitle title_1 title_2 = showTitle title_1 == showTitle title_2 


-- 2
-- test for custom title, returns true if custom type is used
total
isOther : Title -> Bool
isOther (Other _) = True
isOther _ = False


-- 3
data LoginError = InvalidUsername String 
                | InvalidPassword 
                | InvalidKey


-- 4
-- returns string representation of errors
total
showError : LoginError -> String
showError (InvalidUsername username) = "Invalid username: " ++ username
showError InvalidPassword = "Invalid password"
showError InvalidKey = "Invalid key"

