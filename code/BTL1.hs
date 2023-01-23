module BTL1 where

import Lecture3

data Expr
  = Z
  | I Expr
  | Plus Expr Expr
  | Mult Expr Expr
  | Minus Expr Expr
  deriving (Show, Eq, Read)

type ValueDomain = Natural

eval :: Expr -> ValueDomain
eval Z = Zero
eval (I e) = Succ (eval e)
eval (Plus e1 e2) = plus (eval e1) (eval e2)
eval (Mult e1 e2) = mult (eval e1) (eval e2)
eval (Minus Plus e1 e2) = plus (eval e1) (eval e2)
eval (Plus e1 e2) = plus (eval e1) (eval e2)
