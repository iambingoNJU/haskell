
module JoinList where

import Sized
import Scrabble

data JoinList m a = Empty
                | Single m a
                | Append m (JoinList m a) (JoinList m a)
    deriving (Eq, Show)

tag :: Monoid m => (JoinList m a) -> m
tag Empty = mempty
tag (Single m1 _) = m1
tag (Append m1 _ _) = m1

(+++) :: Monoid m => (JoinList m a) -> (JoinList m a) -> (JoinList m a)
jl1 +++ jl2 = Append (tag jl1 `mappend` tag jl2) jl1 jl2

getTagSize :: (Sized b, Monoid b) => (JoinList b a) -> Int
getTagSize = getSize . size . tag

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ i _ | i < 0 = Nothing
indexJ i jl | i >= getTagSize jl = Nothing
indexJ _ (Single _ a) = Just a
indexJ i (Append _ jl1 jl2)
    | i < left = indexJ i jl1
    | otherwise = indexJ (i - left) jl2
    where left = getTagSize jl1

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ i jl | i <= 0 = jl
dropJ i jl | i >= getTagSize jl = Empty
dropJ i (Append m jl1 jl2)
    | i < left = (dropJ i jl1) +++ jl2
    | otherwise = dropJ (i - left) jl2
    where left = getTagSize jl1

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ i _ | i <= 0 = Empty
takeJ i jl | i >= getTagSize jl = jl
takeJ i (Append m jl1 jl2)
    | i <= left = takeJ i jl1
    | otherwise = jl1 +++ (takeJ (i - left) jl2)
    where left = getTagSize jl1

scoreLine :: String -> JoinList Score String
scoreLine str = Single (scoreString str) str

jlToList :: JoinList m a -> [a]
jlToList Empty            = []
jlToList (Single _ a)     = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2
