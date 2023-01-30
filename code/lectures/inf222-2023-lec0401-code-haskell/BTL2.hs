-- | BTL like language version 2: include booleans.
--
-- Magne Haveraen 2023-01-23

module BTL2 where

import Naturals
    ( le, monus, mult, natToInt, one, plus, Natural(..) )

-------------------------
-- | BTL for counting and choice.
data Expr
  = Z
  | I Expr
  | Plus Expr Expr
  | Mult Expr Expr
  | Minus Expr Expr
  | T
  | F
  | And Expr Expr
  | Or Expr Expr
  | Not Expr
  | Le Expr Expr
  | Ifte Expr Expr Expr
  deriving (Show,Eq,Read)


-------------------------
-- | A list of expressions (test cases).
exprList :: [Expr]
exprList = [ expr0, expr1, expr2, expr3, expr4, expr5, expr6, expr7 ]
-- | Some expression
expr0 = Z
expr1 = I $ I $ I $ Z
expr2 = Plus (Mult (I $ I $ Z)(I $ I $ I $ Z)) (I Z)
expr3 = Minus expr2 expr1
expr4 = Mult expr1 (Plus expr2 expr3)
expr5 = Plus (Mult expr0 expr1) (Minus expr2 expr4)
expr6 = Minus (I $ Mult expr4 expr4) (Plus expr4 expr4)
expr7 = Ifte (Le expr1 expr3) expr5 expr6


-------------------------
-- | The semantic domain are the naturals.
-- Encode T as Zero and F as Succ Zero (one)
type ValueDomain = Natural

-- | Evaluator from BTL to the naturals
eval :: Expr -> ValueDomain
eval Z = Zero 
eval (I e) = Succ (eval e)
eval (Plus e1 e2) = plus (eval e1) (eval e2)
eval (Mult e1 e2) = mult (eval e1) (eval e2)
eval (Minus e1 e2) = monus (eval e1) (eval e2)
eval T = Zero
eval F = Succ Zero 
eval (And e1 e2) = if plus (eval e1) (eval e2) == Zero then Zero else Succ Zero
eval (Or e1 e2) = if plus (eval e1) (eval e2) `le` one then Zero else Succ Zero
eval (Not e1) = if eval e1 == Zero then one else Zero
eval (Le e1 e2) = if le (eval e1) (eval e2) then Zero else one
eval (Ifte e0 e1 e2) = if (eval e0) == Zero then (eval e1) else (eval e2)
-- eval _ = error "WRONG"

-------------------------
-- | The semantic domain are the four constructors of Expr: Z I T F.
-- This is called the expression's normal form.
type ValueDomain' = Expr

-- | Evaluator from BTL to the the expression normal form
eval' :: Expr -> ValueDomain'
eval' Z = Z 
eval' (I e) = I (eval' e)
eval' (Plus e1 e2) = plus' (eval' e1) (eval' e2)
eval' (Mult e1 e2) = mult' (eval' e1) (eval' e2)
eval' (Minus e1 e2) = monus' (eval' e1) (eval' e2)
eval' T = T 
eval' F = F 
eval' (And e1 e2) = and' (eval' e1) (eval' e2)
eval' (Or e1 e2) = or' (eval' e1) (eval' e2)
eval' (Not e1) = not' (eval' e1)
eval' (Le e1 e2) = le' (eval' e1) (eval' e2)
eval' (Ifte e0 e1 e2) = ifte' (eval' e0) (eval' e1) (eval' e2)
-- eval' _ = error "WRONG"



-- | Check that an expression is in its normal form.
is_normal_form :: Expr -> Bool
is_normal_form Z = True 
is_normal_form (I e) = is_normal_form e
is_normal_form T = True 
is_normal_form F = True 
is_normal_form e = False
-- is_normal_form e = error $ "Not in normal form: e="++(show e)

plus' :: Expr -> Expr -> Expr
plus' Z e2 | is_normal_form e2 = e2 
plus' (I e1) e2 = I (plus' e1 e2)
plus' e1 e2 = error $ "Subarguments are not in normal form: e1=" ++ (show e1) ++ " e2=" ++ (show e2)

mult' :: Expr -> Expr -> Expr
mult' Z e2 = Z 
mult' (I e1) e2 = (mult' e1 e2) `plus'` e2
mult' e1 e2 = error $ "Subarguments are not in normal form: e1=" ++ (show e1) ++ " e2=" ++ (show e2)

monus' :: Expr -> Expr -> Expr
monus' Z e2 | is_normal_form e2 = Z 
monus' (I e1) (I e2) = monus' e1 e2
monus' e1 Z | is_normal_form e1 = e1 
monus' e1 e2 = error $ "Subarguments are not in normal form: e1=" ++ (show e1) ++ " e2=" ++ (show e2)

and' :: Expr -> Expr -> Expr
and' T T = T
and' _ _ = F
-- and' e1 e2 = error $ "Subarguments are not in normal form: e1=" ++ (show e1) ++ " e2=" ++ (show e2)

or' :: Expr -> Expr -> Expr
or' T T = T
or' T F = T
or' F T = T
or' F F = F
or' e1 e2 = error $ "Subarguments are not in normal form: e1=" ++ (show e1) ++ " e2=" ++ (show e2)

not' :: Expr -> Expr
not' T = F
not' F = T
not' e1 = error $ "Subarguments are not in normal form: e1=" ++ (show e1)

le' :: Expr -> Expr -> Expr
le' Z e2 | is_normal_form e2 = T
le' e1 Z | is_normal_form e1 = F
le' (I e1) (I e2) = le' e1 e2
le' e1 e2 = error $ "Subarguments are not in normal form: e1=" ++ (show e1) ++ " e2=" ++ (show e2)

ifte' :: Expr -> Expr -> Expr -> Expr
ifte' T e1 e2 | is_normal_form e1 = e1
ifte' F e1 e2 | is_normal_form e2 = e2
ifte' e0 e1 e2 = error $ "Subarguments are not in normal form: e0=" ++ (show e0) ++ " e1=" ++ (show e1) ++ " e2=" ++ (show e2)


-------------------------
-- | The semantic domain are the integers and booleans
data ValueDomain'' = VI Integer | VB Bool 
  deriving (Show,Eq,Read)

-- | Evaluator from BTL to the integers and booleans value domain
eval'' :: Expr -> ValueDomain''
eval'' Z = VI 0 
eval'' (I e) = VI (1 + e') 
  where VI e' = eval'' e
eval'' (Plus e1 e2) = VI (e1' + e2')
  where
    VI e1' = eval'' e1
    VI e2' = eval'' e2
eval'' (Mult e1 e2) = VI (e1' * e2')
  where
    VI e1' = eval'' e1
    VI e2' = eval'' e2
eval'' (Minus e1 e2) = VI (if e1' <= e2' then 0 else e1' - e2')
  where
    VI e1' = eval'' e1
    VI e2' = eval'' e2
eval'' T = VB True
eval'' F = VB False 
eval'' (And e1 e2) = VB (e1' && e2')
  where
    VB e1' = eval'' e1
    VB e2' = eval'' e2
eval'' (Or e1 e2) = VB (e1' || e2')
  where
    VB e1' = eval'' e1
    VB e2' = eval'' e2
eval'' (Not e1) = VB (not e1')
  where
    VB e1' = eval'' e1
eval'' (Le e1 e2) = VB (e1' <= e2')
  where
    VI e1' = eval'' e1
    VI e2' = eval'' e2
eval'' (Ifte e0 e1 e2) = if e0' then (eval'' e1) else (eval'' e2)
  where
    VB e0' = eval'' e0
-- eval'' _ = error "WRONG"

-- | Translate to pure integers, encode True as zero and False as one.
trInteger'' :: ValueDomain'' -> Integer
trInteger'' (VI i) = i
trInteger'' (VB True) = 0
trInteger'' (VB False) = 1


-------------------------
-- | Tests for the semantic domains
test_evaluations :: IO ()
test_evaluations = do
  -- Evaluate exprList with eval for the naturals, print result as integers.
  let resInt = map natToInt $ map eval exprList
  print $ "Evaluated test expressions with natToInt.eval: " ++ (show $ resInt)
  -- Evaluate exprList with eval for the expression normal form, compare with eval's result.
  let res' = map eval' exprList
  print $ if all is_normal_form res' then "Evaluator eval' provides normal forms." else "ERROR: evaluator eval' does not achieve normal forms."
  print $ if map (natToInt.eval) res' == resInt then "Evaluator eval' is correct." else "ERROR in implementation of eval'"
  -- Evaluate exprList with eval'' for the integers, compare with eval.
  let res'' = map eval'' exprList
  print $ if map trInteger'' res'' == resInt then "Evaluator eval'' is correct." else "ERROR in implementation of eval''"
  print "end"

