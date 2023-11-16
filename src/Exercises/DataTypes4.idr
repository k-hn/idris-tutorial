module Exercises.DataTypes4

-- 1
total
mapMaybe : (a -> b) -> Maybe a -> Maybe b
mapMaybe fun Nothing = Nothing
mapMaybe fun (Just x) = Just (fun x)

total
appMaybe : Maybe (a -> b) -> Maybe a -> Maybe b
appMaybe (Just _) Nothing = Nothing
appMaybe Nothing _ = Nothing
appMaybe (Just fun) (Just x) = Just (fun x)

total
bindMaybe : Maybe a -> (a -> Maybe b) -> Maybe b
bindMaybe Nothing fun = Nothing
bindMaybe (Just x) fun = fun x

total 
filterMaybe : (a -> Bool) -> Maybe a -> Maybe a
filterMaybe fun Nothing = Nothing
filterMaybe fun (Just x) = if fun x then Just x else Nothing

total
first : Maybe a -> Maybe a -> Maybe a
first Nothing Nothing = Nothing
first (Just x) _ = Just x
first _ (Just y) = Just y

total
last : Maybe a -> Maybe a -> Maybe a
last Nothing Nothing = Nothing
last _ (Just y) = Just y
last (Just x) _ = Just x

total
foldMaybe : (acc -> el -> acc) -> acc -> Maybe el -> acc
foldMaybe _ x Nothing = x
foldMaybe fun x (Just y) = fun x y


-- 2
total
mapEither : (a -> b) -> Either e a -> Either e b
mapEither _ (Left x) = Left x
mapEither fun (Right y) = Right (fun y)

total
appEither : Either e (a -> b) -> Either e a -> Either e b
appEither (Right f) (Right y) = Right (f y)
appEither (Left e) _ = Left e
appEither _ (Left x) = Left x

total
bindEither : Either e a -> (a -> Either e b) -> Either e b
bindEither (Right y) fun = fun y
bindEither (Left x) _ = Left x

total
firstEither : (e -> e -> e) -> Either e a -> Either e a -> Either e a
firstEither _ (Right y) _ = Right y
firstEither _ _ (Right y) = Right y
firstEither f (Left m) (Left n) = Left (f m n)

total
lastEither : (e -> e -> e) -> Either e a -> Either e a -> Either e a
lastEither f x y = firstEither (flip f) y x

total
fromEither : (e -> c) -> (a -> c) -> Either e a -> c
fromEither f _ (Left x) = f x
fromEither _ g (Right y) = g y


-- 3
total
mapList : (a -> b) -> List a -> List b
mapList _ Nil = Nil
mapList fun (x :: xs) = (fun x) :: mapList fun xs

total
filterList : (a -> Bool) -> List a -> List a
filterList f Nil = Nil
filterList f (x :: xs) = if f x then x :: filterList f xs else filterList f xs

total
headMaybe : List a -> Maybe a
headMaybe Nil = Nothing
headMaybe (x :: xs) = Just x

total
tailMaybe : List a -> Maybe (List a)
tailMaybe Nil = Nothing
tailMaybe (_ :: Nil) = Nothing
tailMaybe (x :: xs) = Just xs

total
lastMaybe : List a -> Maybe a
lastMaybe Nil = Nothing
lastMaybe (x :: Nil) = Just x
lastMaybe (x :: xs) = lastMaybe xs


initMaybeHelper : List a -> List a
initMaybeHelper Nil = Nil
initMaybeHelper (x :: Nil) = Nil
initMaybeHelper (x :: xs) = x :: initMaybeHelper xs

total
initMaybe : List a -> Maybe (List a)
initMaybe ls = 
  case initMaybeHelper ls of
    Nil => Nothing
    ls => Just ls

total
foldList : (acc -> el -> acc) -> acc -> List el -> acc
foldList _ acc Nil = acc
foldList fun acc (x :: xs) = foldList fun (fun acc x) xs


-- 4
data Title = Mr | Mrs | Other String

data Credentials = Password String Bits64 | Key String String

record Client where
  constructor MkClient
  name : String
  title : Title
  age : Bits8
  passwordOrKey : Either Bits64 String

data LoginError = InvalidUsername String 
                | InvalidPassword 
                | InvalidKey

login : List Client -> Credentials -> Either LoginError Client
login Nil (Password _ _) = Left InvalidPassword
login Nil (Key _ _) = Left InvalidKey
login (client :: clients) cred@(Password username pass) =
  case client.passwordOrKey of
    Left x => if x == pass then Right client else login clients cred
    Right _ => login clients cred  
login (client :: clients) cred@(Key username key) =
  case client.passwordOrKey of
    Right x => if x == key then Right client else login clients cred 
    Left _ => login clients cred


-- 5
data Element = H | C | N | O | F

ethanol : List (Element,Nat)
ethanol = [(C,2),(H,6),(O,1)]

atomicMass : Element -> Double
atomicMass H = 1.008
atomicMass C = 12.011
atomicMass N = 14.007
atomicMass O = 15.999
atomicMass F = 18.9984

molarMass : List (Element, Nat) -> Double
molarMass Nil = 0.0
molarMass ((elem, moles) :: xs) = 
  atomicMass elem * cast moles + molarMass xs  
