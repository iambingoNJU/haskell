
module Golf where

takeEveryN :: [a] -> Int -> [a]
takeEveryN [] _ = []
takeEveryN l n = (take 1 (drop (n - 1) l)) ++ (takeEveryN (drop n l) n)

skips :: [a] -> [[a]]
skips l = map (takeEveryN l) [1..(length l)]


midmax :: Integer -> Integer -> Integer -> Bool
midmax a b c = (b > a) && (b > c)

localMaxima :: [Integer] -> [Integer]
localMaxima (a:b:c:xs)
    | midmax a b c = b : localMaxima (b:c:xs)
    | otherwise = localMaxima (b:c:xs)
localMaxima _ = []


getCnt :: [Integer] -> [Integer]
getCnt l = map (\i -> fromIntegral (length (filter (== i) l))) [0..9]

his :: [Integer] -> [String]
his l = map (\i -> map (\j -> if i + j > maximum l then '*' else ' ') l) [1..maximum l]

histogram :: [Integer] -> String
histogram l = unlines ((his (getCnt l) ++ ["==========", "0123456789"]))

