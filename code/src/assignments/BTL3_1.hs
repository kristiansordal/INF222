module BTL3_1 where

import BTL3 (Expr)

data Expr'
  = Z
  | I Expr
  | T
  | F

type ValueDomain' = Expr'

eval' :: Expr -> ValueDomain'
eval' (Z :: Expr) = Z
eval' (I e) = (I (eval' e))
