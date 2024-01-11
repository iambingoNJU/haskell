{-# LANGUAGE TypeSynonymInstances #-}

module Compile where

import StackVM
import Parser
import Expr

instance Expr Program where
    lit x = [PushI x]
    add a b = a ++ b ++ [Add]
    mul a b = a ++ b ++ [Mul]

compile :: String -> Maybe Program
compile = parseExp lit add mul
