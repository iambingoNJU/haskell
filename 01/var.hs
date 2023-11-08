
x :: Int
x = 3

-- comment
y :: Int
y = y + 1

d1, d2 :: Double
d1 = 3.14
d2 = 2.3e-2


fac :: Integer -> Integer
fac 0 = 0
fac n = n + fac(n-1)

isEven :: Integer -> Bool
isEven n = n `mod` 2 == 0
