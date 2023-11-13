module Tutorial.Intro

main : IO ()
main = putStrLn "Hello World!"

maxBits8 : Bits8
maxBits8 = 255

distanceToMax : Bits8 -> Bits8
distanceToMax n = maxBits8 - n

square : Integer -> Integer
square n = n * n
