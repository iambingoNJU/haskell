
module Scrabble where

import Data.Char

newtype Score = Score Int
    deriving (Eq, Ord, Show, Num)

getScore :: Score -> Int
getScore (Score i) = i

instance Semigroup Score where
    Score a <> Score b = Score (a + b)

instance Monoid Score where
    mempty = Score 0

score :: Char -> Score
score ch
    | ch `elem` ['a'..'z'] = Score (scs !! (fromEnum ch - fromEnum 'a'))
    | ch `elem` ['A'..'Z'] = Score (scs !! (fromEnum ch - fromEnum 'A'))
    | otherwise = Score 0
    where scs = [1,3,3,2,1,4,2,4,1,8,5,1,3,1,1,3,10,1,1,1,1,4,4,8,4,10]

scoreString :: String -> Score
scoreString = mconcat . map score
