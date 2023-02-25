{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use first" #-}
module Pam4Eval where

import Pam4State
import Pam4VariableAST

eval :: CalcExprAST -> State -> Integer
eval (Lit x) _ = x
eval (Add x y) s = eval x s + eval y s
eval (Mult x y) s = eval x s * eval y s
eval (Sub x y) s = eval x s - eval y s
eval (Neg x) s = negate (eval x s)
eval (Var vname) (x, y)
  | isDeclared vname (x, y) = getValue vname (x, y)
  | otherwise = error $ "Variable not declared: " ++ show vname

exec :: CalcStmtAST -> State -> State
exec (SetVar vname expr) s = addVariable vname (eval expr s) s
exec (AssVar vname expr) s = changeValue vname (eval expr s) s
