-- | BTL like language version 1: simple numerical operations.
-- Focus on the naturals as the semantic domain.
--
-- Magne Haveraen 2023-01-23
module BTL1 where

-- Import only the part of Naturals that we need.

import Data.Int (Int16, Int8)
import Data.Word (Word16, Word8)
import Naturals
  ( Natural (..),
    intToNat,
    monus,
    mult,
    natToInt,
    plus,
  )

-------------------------

-- | BTL for counting and other operations.
data Expr
  = Z
  | I Expr
  | Plus Expr Expr
  | Mult Expr Expr
  | Minus Expr Expr
  deriving (Show, Eq, Read)

-------------------------

-- | A list of expressions (test cases)
exprList :: [Expr]
exprList = [expr0, expr1, expr2, expr3, expr4, expr5, expr6, expr7, expr8, expr9]

-- | Some expression
expr0 = Z

expr1 = I $ I $ I Z

expr2 = Plus (Mult (I $ I Z) (I $ I $ I Z)) (I Z)

expr3 = Minus expr2 expr1

expr4 = Mult expr1 (Plus expr2 expr3)

expr5 = Plus (Mult expr0 expr1) (Minus expr2 expr4)

expr6 = Minus (I $ Mult expr4 expr4) (Plus expr4 expr4)

expr7 = I $ I $ I $ I $ I $ I Z

expr8 = Plus expr2 (Mult expr1 expr2)

expr9 = Plus expr2 (Mult expr2 expr2)

-------------------------

-- | The semantic domain is the naturals.
type ValueDomain = Natural

-- | Evaluator from BTL to the naturals
eval :: Expr -> ValueDomain
eval Z = Zero
eval (I e) = Succ (eval e)
eval (Plus e1 e2) = plus (eval e1) (eval e2)
eval (Mult e1 e2) = mult (eval e1) (eval e2)
eval (Minus e1 e2) = monus (eval e1) (eval e2)

-------------------------

-- | The semantic domain is the first two constructors of Expr.
-- This is called the expression's normal form.
type ValueDomain' = Expr

-- | Evaluator from BTL to the the expression normal form
eval' :: Expr -> ValueDomain'
eval' Z = Z
eval' (I e) = I (eval' e)
eval' (Plus e1 e2) = plus' (eval' e1) (eval' e2)
eval' (Mult e1 e2) = mult' (eval' e1) (eval' e2)
eval' (Minus e1 e2) = monus' (eval' e1) (eval' e2)

-- | Check that an expression is in its normal form.
is_normal_form :: Expr -> Bool
is_normal_form Z = True
is_normal_form (I e) = is_normal_form e
is_normal_form e = False

-- is_normal_form e = error $ "Not in normal form: e="++(show e)

plus' :: Expr -> Expr -> Expr
plus' Z e2 | is_normal_form e2 = e2
plus' (I e1) e2 = I (plus' e1 e2)
plus' e1 e2 = error $ "Subarguments are not in normal form: e1=" ++ show e1 ++ " e2=" ++ show e2

mult' :: Expr -> Expr -> Expr
mult' Z e2 = Z
mult' (I e1) e2 = mult' e1 e2 `plus'` e2
mult' e1 e2 = error $ "Subarguments are not in normal form: e1=" ++ show e1 ++ " e2=" ++ show e2

monus' :: Expr -> Expr -> Expr
monus' Z e2 | is_normal_form e2 = Z
monus' (I e1) (I e2) = monus' e1 e2
monus' e1 Z | is_normal_form e1 = e1
monus' e1 e2 = error $ "Subarguments are not in normal form: e1=" ++ show e1 ++ " e2=" ++ show e2

-------------------------

-- | The semantic domain is the integers.
type ValueDomain'' = Integer

-- | Evaluator from BTL to the integers
eval'' :: Expr -> ValueDomain''
eval'' Z = 0
eval'' (I e) = 1 + eval'' e
eval'' (Plus e1 e2) = eval'' e1 + eval'' e2
eval'' (Mult e1 e2) = eval'' e1 * eval'' e2
eval'' (Minus e1 e2) = res'
  where
    res = eval'' e1 - eval'' e2
    res' = if res < 0 then 0 else res

-------------------------

-- | The semantic domain are the 8bit signed integers.
type ValueDomain''' = Int8

-- | Evaluator from BTL to Int8
eval''' :: Expr -> ValueDomain'''
eval''' Z = 0
eval''' (I e) = 1 + eval''' e
eval''' (Plus e1 e2) = eval''' e1 + eval''' e2
eval''' (Mult e1 e2) = eval''' e1 * eval''' e2
eval''' (Minus e1 e2) = res'
  where
    res = eval''' e1 - eval''' e2
    res' = if res < 0 then 0 else res

-------------------------

-- | The semantic domain are the 16bit unsigned integers.
type ValueDomain'''' = Word16

-- | Evaluator from BTL to Word16
eval'''' :: Expr -> ValueDomain''''
eval'''' Z = 0
eval'''' (I e) = 1 + eval'''' e
eval'''' (Plus e1 e2) = eval'''' e1 + eval'''' e2
eval'''' (Mult e1 e2) = eval'''' e1 * eval'''' e2
eval'''' (Minus e1 e2) = res'
  where
    e1' = eval'''' e1
    e2' = eval'''' e2
    res' = if e1' <= e2' then 0 else e1' - e2'

-------------------------

-- | Tests for the semantic domains
test_evaluations :: IO ()
test_evaluations = do
  -- Evaluate exprList with eval for the naturals, print result as integers.
  let resInt = map (natToInt . eval) exprList
  print $ "Evaluated test expressions with natToInt.eval: " ++ (show resInt)
  -- Evaluate exprList with eval for the expression normal form, compare with eval's result.
  let res' = map eval' exprList
  print $ if all is_normal_form res' then "Evaluator eval' provides normal forms." else "ERROR: evaluator eval' does not achieve normal forms."
  print $ if map (natToInt . eval) res' == resInt then "Evaluator eval' is correct." else "ERROR in implementation of eval'"
  -- Evaluate exprList with eval'' for the integers, compare with eval.
  let res'' = map eval'' exprList
  print $ if res'' == resInt then "Evaluator eval'' is correct." else "ERROR in implementation of eval''"
  -- Evaluate exprList with eval''' for wrapping signed integers, compare with eval.
  let res''' = map eval''' exprList
  print $ if map toInteger res''' == resInt then "Evaluator eval''' is correct." else "ERROR in implementation of eval'''"
  -- Evaluate exprList with eval'''' for wrapping unsigned integers, compare with eval.
  let res'''' = map eval'''' exprList
  print $ if map toInteger res'''' == resInt then "Evaluator eval'''' is correct." else "ERROR in implementation of eval''''"
  print "end"
