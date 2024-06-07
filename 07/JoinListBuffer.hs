module JoinListBuffer where

import Data.Monoid

import Buffer
import JoinList
import Scrabble
import Sized

instance Monoid m => Monoid (JoinList m a) where
    mempty = Empty

instance Monoid m => Semigroup (JoinList m a) where
    jl1 <> jl2 = jl1 +++ jl2

instance Buffer (JoinList (Score, Size) String) where
  toString     = unlines . jlToList
  fromString   = mconcat . map createJl . lines
                where createJl str = Single (scoreString str, Size 1) str
  line         = indexJ
  replaceLine n l b = takeJ n b +++ fromString l +++ dropJ (n + 1) b
  numLines     = getSize . snd . tag
  value        = getScore . fst . tag