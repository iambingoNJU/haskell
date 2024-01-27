
module Fibonacci where

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = (fib (n - 1)) + (fib (n - 2))

fibs1 :: [Integer]
fibs1 = map fib [0..]

fibs2 :: [Integer]
fibs2 = 0 : 1 : zipWith (+) fibs2 (tail fibs2)


data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons a b) = a : (streamToList b)

instance Show a => Show (Stream a) where
    show x = show $ take 20 (streamToList x)


streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons (f x) (streamMap f xs)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons x (streamFromSeed f (f x))


nats :: Stream Integer
nats = streamFromSeed (+1) 0

ruler :: Stream Integer
ruler = startRuler 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons x xs) y = Cons x (interleaveStreams y xs)

startRuler :: Integer -> Stream Integer
startRuler x = interleaveStreams (streamRepeat x) (startRuler (x+1))
