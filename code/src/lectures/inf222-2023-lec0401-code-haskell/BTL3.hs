-- | BTL like language version 3: literals as strings.
--
-- Magne Haveraen 2023-01-23

module BTL3 where

import Naturals
    ( le, monus, mult, natToInt, intToNat, one, plus, Natural(..) )

-------------------------
-- | BTL for counting and choice.
data Expr
  = LitNat String
  | Plus Expr Expr
  | Mult Expr Expr
  | Minus Expr Expr
  | LitBool String
  | And Expr Expr
  | Or Expr Expr
  | Not Expr
  | Le Expr Expr
  | Ifte Expr Expr Expr
  deriving (Show,Eq,Read)


-------------------------
-- | A list of expressions (test cases).
-- The arguments should be strings corresponding to the literals 0 1 2 3 (in the chosen semantics), when possible.
exprList :: String -> String -> String -> String -> [Expr]
exprList str0 str1 str2 str3 = 
  [ expr0 str0, 
    expr1 str3, 
    expr2 str1 str2 str3, 
    expr3 str1 str2 str3, 
    expr4 str1 str2 str3, 
    expr5 str0 str1 str2 str3, 
    expr6 str1 str2 str3, 
    expr7 str0 str1 str2 str3]
-- | Some expression
expr0 str0 = LitNat str0
expr1 str3 = LitNat str3
expr2 str1 str2 str3 = Plus (Mult (LitNat str2)(LitNat str3)) (LitNat str1)
expr3 str1 str2 str3 = Minus (expr2 str1 str2 str3) (expr1 str3)
expr4 str1 str2 str3 = Mult (expr1 str3) (Plus (expr2 str1 str2 str3) (expr3 str1 str2 str3))
expr5 str0 str1 str2 str3= Plus (Mult (expr0 str0) (expr1 str3)) (Minus (expr2 str1 str2 str3) (expr4 str1 str2 str3))
expr6 str1 str2 str3 = Minus (Plus (LitNat str1) (Mult expr4' expr4')) (Plus expr4' expr4')
  where
    expr4' = expr4 str1 str2 str3
expr7 str0 str1 str2 str3 = Ifte (Le (expr1 str3) (expr3 str1 str2 str3)) (expr5 str0 str1 str2 str3) (expr6 str1 str2 str3)


-------------------------
-- | The semantic domain are the naturals.
-- Encode T as Zero and F as Succ Zero (i.e., one)
type ValueDomain = Natural

-- | Evaluator from BTL to the naturals
eval :: Expr -> ValueDomain
eval (LitNat str) = read str
eval (Plus e1 e2) = plus (eval e1) (eval e2)
eval (Mult e1 e2) = mult (eval e1) (eval e2)
eval (Minus e1 e2) = monus (eval e1) (eval e2)
eval (LitBool "True") = Zero
eval (LitBool "False") = Succ Zero 
eval (And e1 e2) = if plus (eval e1) (eval e2) == Zero then Zero else Succ Zero
eval (Or e1 e2) = if plus (eval e1) (eval e2) `le` one then Zero else Succ Zero
eval (Not e1) = if eval e1 == Zero then one else Zero
eval (Le e1 e2) = if le (eval e1) (eval e2) then Zero else one
eval (Ifte e0 e1 e2) = if (eval e0) == Zero then (eval e1) else (eval e2)
-- eval _ = error "WRONG"


-------------------------
-- | The semantic domain are the integers and booleans
data ValueDomain'' = VI Integer | VB Bool 
  deriving (Show,Eq,Read)

-- | Evaluator from BTL to the value domain of integers and booleans
eval'' :: Expr -> ValueDomain''
eval'' (LitNat i) = VI (read i) 
eval'' (Plus e1 e2) = VI (e1' + e2')
  where
    VI e1' = eval'' e1
    VI e2' = eval'' e2
eval'' (Mult e1 e2) = VI (e1' * e2')
  where
    VI e1' = eval'' e1
    VI e2' = eval'' e2
eval'' (Minus e1 e2) = VI (e1' - e2')
  where
    VI e1' = eval'' e1
    VI e2' = eval'' e2
eval'' (LitBool str) = VB (read str)
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
-- | The semantic domain are 3D integer vectors (lists) and booleans
data ValueDomain''' = VVI [Integer] | VVB Bool 
  deriving (Show,Eq,Read)

-- | Evaluator from BTL to the vector and booleans value domain.
-- All opwerations are componentwise, except multiplication which is the vector corss product.
--   https://en.wikipedia.org/wiki/Cross_product
eval''' :: Expr -> ValueDomain'''
eval''' (LitNat v) = VVI v''
  where
    v' = read v
    v'' = if length v' == 3 then v' else error $ "Wrong length of input, v'="++(show v')
eval''' (Plus e1 e2) = VVI (map (\(x,y)->x+y) $ zip e1' e2')
  where
    VVI e1' = eval''' e1
    VVI e2' = eval''' e2
eval''' (Mult e1 e2) = VVI [s1,s2,s3]
  where
    VVI [a1,a2,a3] = eval''' e1
    VVI [b1,b2,b3] = eval''' e2
    s1 = a2*b3 - a3 * b2
    s2 = a3*b1 - a1*b3
    s3 = a1*b2 - a2*b1
eval''' (Minus e1 e2) = VVI (map ((\(x,y)->x-y)) $ zip e1' e2')
  -- Minus: VVI (map (\(x,y)->if x <= y then 0 else x-y) $ zip e1' e2')
  where
    VVI e1' = eval''' e1
    VVI e2' = eval''' e2
eval''' (LitBool str) = VVB (read str)
eval''' (And e1 e2) = VVB (e1' && e2')
  where
    VVB e1' = eval''' e1
    VVB e2' = eval''' e2
eval''' (Or e1 e2) = VVB (e1' || e2')
  where
    VVB e1' = eval''' e1
    VVB e2' = eval''' e2
eval''' (Not e1) = VVB (not e1')
  where
    VVB e1' = eval''' e1
eval''' (Le e1 e2) = VVB (all (\(x,y)->x <= y) $ zip e1' e2')
  where
    VVI e1' = eval''' e1
    VVI e2' = eval''' e2
eval''' (Ifte e0 e1 e2) = if e0' then (eval''' e1) else (eval''' e2)
  where
    VVB e0' = eval''' e0
-- eval''' _ = error "WRONG"



-------------------------
-- | Tests for the semantic domains
test_evaluations :: IO ()
test_evaluations = do
  -- Evaluate exprList with eval for the naturals, print result as integers.
  let resInt = map natToInt $ map eval (exprList "Zero" "Succ Zero" "Succ (Succ Zero)" "Succ (Succ (Succ Zero))")
  print $ "Evaluated test expressions with natToInt.eval: " ++ (show $ resInt)
  -- Evaluate exprList with eval'' for the integers, compare with eval.
  let res'' = map eval'' (exprList "0" "1" "2" "3")
  print $ if map trInteger'' res'' == resInt then "Evaluator eval'' is correct." else "ERROR in implementation of eval''"
  -- Evaluate exprList with eval''' for the vectors.
  let res''' = map eval''' (exprList "[0,1,3]" "[2,1,5]" "[3,-1,4]" "[-3,5,-7]")
  print $ "Vector computations: " ++ (show res''')
  print "end"
