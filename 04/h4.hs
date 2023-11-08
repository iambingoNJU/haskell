
----------------- Ex1
fun1 :: [Integer] -> Integer
fun1 = product . map (subtract 2) . filter even

magic :: Integer -> Integer
magic x = if (even x) then x `div` 2 else 3 * x + 1

fun2 :: Integer -> Integer
fun2 = sum . filter even . takeWhile (>1) . iterate magic

--f 10 = 10 + f 5 = 10 + f 16 = 10 + 16 + f 8 = 10 + 16 + 8 + f 4 = 10 + 16 + 8 + 4 + 2 + 0

----------------- Ex2
data Tree a = Leaf
             | Node Integer (Tree a) a (Tree a)
   deriving (Show, Eq)

log2d :: Double -> Integer
log2d = floor . logBase 2

foldTree :: [a] -> Tree a
foldTree [] = Leaf
foldTree xs = let first = length xs `div` 2
                  left = drop first xs
    in Node (log2d (fromIntegral (length xs))) (foldTree (take first xs)) (head left) (foldTree (drop 1 left))

height :: Tree a -> Integer
height Leaf = -1
height (Node h _ _ _) = h

insertTree :: a -> Tree a -> Tree a
insertTree a Leaf = Node 0 Leaf a Leaf
insertTree a (Node h l m r)
    | h1 < h2 = Node h (insertTree a l) m r
    | h1 > h2 = Node h l m (insertTree a r)
    | otherwise = Node (h3 + 1) l' m r
    where h1 = height l
          h2 = height r
          l' = insertTree a l
          h3 = height l'

foldTree2 :: [a] -> Tree a
foldTree2 = foldr insertTree Leaf

------------------- Ex3
xor :: [Bool] -> Bool
xor = foldr (\x y -> if x then not y else y) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x xs -> (f x):xs) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (\x y z -> y (f z x)) id xs base

-- foldr f z [x1, x2, ..., xn] == x1 ‘f‘ (x2 ‘f‘ ... (xn ‘f‘ z)...)
-- foldl f z [x1, x2, ..., xn] == (...((z ‘f‘ x1) ‘f‘ x2) ‘f‘...) ‘f‘ xn

------------------- Ex4
sieveSundaram :: Integer -> [Integer]
sieveSundaram x = map (\x -> 2 * x + 1) (filter (`notElem` (takeWhile (<x) [x+y+2*x*y | x <- [1..x], y <- [1..x]])) [1..x])
