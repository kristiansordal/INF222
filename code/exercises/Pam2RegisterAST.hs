{-# LANGUAGE BlockArguments #-}

-- | AST for register based integer calculator.
--
-- Author Magne Haveraaen
-- Since 2020-03-14
module Pam2RegisterAST where

import Data.Char
import Pam2RegisterStore

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
data CalcStmtAST = SetReg Register CalcExprAST
  deriving (Eq, Read, Show)

-- | Enumeration of the 10 registers.
data Register
  = Reg0
  | Reg1
  | Reg2
  | Reg3
  | Reg4
  | Reg5
  | Reg6
  | Reg7
  | Reg8
  | Reg9
  deriving (Eq, Read, Show)

-----------------------

-- | A few ASTs for register based CalcExprAST.
calculatorRegisterAST1 = Lit 4

calculatorRegisterAST2 = Neg (Mult (Add (Lit 3) (Sub (Lit 7) (Lit 13))) (Lit 19))

calculatorRegisterAST3 = Add (Reg Reg1) (Reg Reg4)

calculatorRegisterAST4 = Reg Reg2

-- | A few ASTs for setting registers CalcStmtAST.
calculatorSetRegisterAST1 = SetReg Reg4 calculatorRegisterAST1

calculatorSetRegisterAST2 = SetReg Reg1 calculatorRegisterAST2

calculatorSetRegisterAST3 = SetReg Reg2 calculatorRegisterAST3

calculatorSetRegisterAST4 = SetReg Reg1 calculatorRegisterAST4

eval :: CalcExprAST -> Store -> Integer
eval (Lit x) s = x
eval (Add x y) s = eval x s + eval y s
eval (Mult x y) s = eval x s * eval y s
eval (Sub x y) s = eval x s - eval y s
eval (Neg x) s = negate (eval x s)
eval (Reg x) s = getStore s (getRegisterIndex x)

execute :: CalcStmtAST -> Store -> Store
execute (SetReg r e) s = setStore (getRegisterIndex r) (eval e s) s

getRegisterIndex :: Register -> Integer
getRegisterIndex = fromIntegral . digitToInt . last . show

test :: IO ()
test = do
  let regs = setStore 1 (eval calculatorRegisterAST1 registerStore) registerStore
      regs' = setStore 2 (eval calculatorRegisterAST2 regs) regs
      regs'' = setStore 3 (eval calculatorRegisterAST3 regs') regs'
      regs''' = setStore 4 (eval calculatorRegisterAST4 regs'') regs''

  -- evals = map (`eval` regs) [calculatorRegisterAST1, calculatorRegisterAST2, calculatorRegisterAST3, calculatorRegisterAST4]
  print regs'''

main = do
  putStrLn "−−␣ Interactive ␣ register ␣ calulator ␣−−"
  runInputT defaultSettings (loop registerStore)
  where
    loop :: Store -> InputT IO ()
    loop state = do
      input <- getInputLine
        "c|␣"
        case input of
          Nothing -> return ()
          Just "" ->
            do outputStrLn "Finished "; return ()
          Just "show" ->
            do outputStrLn $ " state ␣=␣" ++ show state; loop state
          Just str -> do
            case readMaybe str :: Maybe CalcStmtAST of
              Nothing -> do
                outputStrLn $ "Not␣a␣statement:␣" ++ show str
                loop state
              Just stmt ->
                let SetReg reg expr = stmt
                 in do
                      outputStrLn $ show reg ++ "␣=␣" ++ show (evaluate expr state)
                      loop
                      $ execute stmt state
      print ""
