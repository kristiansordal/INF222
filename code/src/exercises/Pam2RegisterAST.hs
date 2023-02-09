{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

-- | AST for register based integer calculator.
--
-- Author Magne Haveraaen
-- Since 2020-03-14
module Pam2RegisterAST where

import Data.Char
import Pam2RegisterStore
import System.Console.Haskeline
import Text.Read

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
eval (Lit x) _ = x
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
  let store = setStore (getRegisterIndex Reg4) 43 (setStore (getRegisterIndex Reg2) 21 (setStore (getRegisterIndex Reg1) 11 registerStore))
      store1 = execute (SetReg Reg1 (Lit 11)) registerStore
      store2 = execute (SetReg Reg2 (Add (Reg Reg1) (Lit 10))) store1
      store3 = execute (SetReg Reg4 (Sub (Mult (Reg Reg2) (Lit 2)) (Lit (-1)))) store2
      expected = [4, 57, 54, 21]
      evals = map (`eval` store3) [calculatorRegisterAST1, calculatorRegisterAST2, calculatorRegisterAST3, calculatorRegisterAST4]
      correct = all ((== True) . uncurry (==)) (zip expected evals)

  if correct
    && (store == store3)
    && (57 == eval (Reg Reg1) (execute calculatorSetRegisterAST2 registerStore))
    && (0 == eval (Reg Reg2) (execute calculatorSetRegisterAST3 registerStore))
    && (0 == eval (Reg Reg1) (execute calculatorSetRegisterAST4 registerStore))
    then putStrLn "Unit Tests Hold"
    else putStrLn "Test failed"

main =
  do
    putStrLn "Interative Register Calculator"
    runInputT defaultSettings (loop registerStore)
  where
    loop :: Store -> InputT IO ()
    loop state = do
      input <- getInputLine "c|"
      case input of
        Nothing -> return ()
        Just "" -> do outputStrLn "Finished"; return ()
        Just "show" -> do
          outputStrLn $ "state = " ++ show state
          loop state
        Just str -> do
          outputStrLn (show str)
          case (readMaybe str :: Maybe CalcStmtAST) of
            Nothing -> do
              outputStrLn $ "Not a statement: " ++ show str
              loop state
            Just stmt -> do
              let SetReg reg expr = stmt
              outputStrLn $ show reg ++ " = " ++ show (eval expr state)
              loop $ execute stmt state
