
module Main where

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
    | n <= 0 = []
    | n < 10 = [n]
    | otherwise = (n `mod` 10) : toDigitsRev (n `div` 10)

toDigits :: Integer -> [Integer]
toDigits n
    | n <= 0 = []
    | n < 10 = [n]
    | otherwise = toDigits (n `div` 10) ++ [n `mod` 10]

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:[]) = [x]
doubleEveryOther (x:y:l) = x : 2 * y : doubleEveryOther l

sum2digits :: Integer -> Integer
sum2digits n
    | n < 10 = n
    | otherwise = n `div` 10 + n `mod` 10

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:l) = sum2digits x + sumDigits l

validate :: Integer -> Bool
validate n = sumDigits (doubleEveryOther (toDigitsRev n)) `mod` 10 == 0

main = do
    print (toDigitsRev 123)
    print (toDigitsRev 0)
    print (toDigitsRev (-34))

    print (toDigits 123)

    print (doubleEveryOther (toDigitsRev 456))

    print (sum2digits 3)
    print (sum2digits 18)

    print (sumDigits [12, 3, 4, 56])

    print (validate 4012888888881881)
    print (validate 4012888888881882)