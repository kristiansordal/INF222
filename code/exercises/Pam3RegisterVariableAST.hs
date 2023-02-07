-- | AST for register based integer calculator.
--
-- Author Magne Haveraaen
-- Since 2020-03-14
module Pam3RegisterVariableAST where

import Data.Map (Map)
import Data.Map qualified as Map

-----------------------

-- | Expressions for a calculator with 10 registers.
-- The calculator supports literals and operations
-- Addition, multiplication, and subtraction/negation.
data CalcExprAST
  = Lit Integer
  | Add CalcExprAST CalcExprAST
  | Mult CalcExprAST CalcExprAST
  | Sub CalcExprAST CalcExprAST
  | Neg CalcExprAST
  | Reg Register
  deriving (Eq, Read, Show)

-- | Statement for setting a register
data CalcStmtAST
  = SetReg Register CalcExprAST
  deriving (Eq, Read, Show)

type Store = Map Register Integer

-- | The register type is now variable names (strings).
type Register = String

registerStore = Map.empty

getStore :: Store -> Register -> Integer
getStore m i = case Map.lookup i m of
  (Just i') -> i'
  Nothing -> error "Not a register index"

setStore :: Register -> Integer -> Store -> Store
setStore = Map.insert

-----------------------

-- | A few ASTs for register based CalcExprAST.
calculatorRegisterAST1 = Lit 4

calculatorRegisterAST2 = Neg (Mult (Add (Lit 3) (Sub (Lit 7) (Lit 13))) (Lit 19))

calculatorRegisterAST3 = Add (Reg "Reg1") (Reg "Reg4")

calculatorRegisterAST4 = Reg "Reg2"

-- | A few ASTs for setting registers CalcStmtAST.
calculatorSetRegisterAST1 = SetReg "Reg4" calculatorRegisterAST1

calculatorSetRegisterAST2 = SetReg "Reg1" calculatorRegisterAST2

calculatorSetRegisterAST3 = SetReg "Reg2" calculatorRegisterAST3

calculatorSetRegisterAST4 = SetReg "Reg1" calculatorRegisterAST4

-----------------------

eval :: CalcExprAST -> Store -> Integer
eval (Lit x) s = x
eval (Add x y) s = eval x s + eval y s
eval (Mult x y) s = eval x s * eval y s
eval (Sub x y) s = eval x s - eval y s
eval (Neg x) s = negate (eval x s)
eval (Reg x) s = getStore s x

test :: IO ()
test = do
  let regs = setStore "Reg0"
      regs' = setStore 2 (eval calculatorRegisterAST2 regs) regs
      regs'' = setStore 3 (eval calculatorRegisterAST3 regs') regs'
      regs''' = setStore 4 (eval calculatorRegisterAST4 regs'') regs''
  print regs'''
