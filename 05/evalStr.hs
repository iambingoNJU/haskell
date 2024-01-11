
module EvalStr where

import Parser
import ExprT
import Eval

evalStr :: String -> Maybe Integer
evalStr str = case parseExp Lit Add Mul str of
    Just x -> Just (eval x)
    Nothing -> Nothing