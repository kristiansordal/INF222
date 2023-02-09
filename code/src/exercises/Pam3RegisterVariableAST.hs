-- | AST for register based integer calculator.
--
-- Author Magne Haveraaen
-- Since 2020-03-14
module Pam3RegisterVariableAST where

import Data.Char
import Data.Map (Map)
import qualified Data.Map as Map
import System.Console.Haskeline
import Text.Read

-- Expressions for a calculator with 10 registers.
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

-- Statement for setting a register
data CalcStmtAST
  = SetReg Register CalcExprAST
  | AddReg Register CalcExprAST
  deriving (Eq, Read, Show)

-- Store is now a map from Integer to Integer, allowing for infinite registers
type Store = Map Integer Integer

-- The register type is now variable names (strings).
type Register = String

-- Initialize the first 10 registers
registerStore = Map.fromList [(i, 0) | i <- [0 .. 9]]

getStore :: Store -> Integer -> Integer
getStore m i = case Map.lookup i m of
  (Just i') -> i'
  Nothing -> error "Not a register index"

addStore :: Store -> Integer -> CalcExprAST -> Store
addStore s r stmt = setStore r (eval stmt s) s

-- Setting a store in the map
setStore = Map.insert

-- Getting the index of a register. Kindof redundant but kept in the spirit of Magne
-- Could be omitted if the Store is a map from string -> integer
getRegisterIndex :: Register -> Integer
getRegisterIndex r = read $ filter isDigit r

-- A few ASTs for register based CalcExprAST.
calculatorRegisterAST1 = Lit 4

calculatorRegisterAST2 = Neg (Mult (Add (Lit 3) (Sub (Lit 7) (Lit 13))) (Lit 19))

calculatorRegisterAST3 = Add (Reg "Reg1") (Reg "Reg4")

calculatorRegisterAST4 = Reg "Reg2"

-- | A few ASTs for setting registers CalcStmtAST.
calculatorSetRegisterAST1 = SetReg "Reg4" calculatorRegisterAST1

calculatorSetRegisterAST2 = SetReg "Reg1" calculatorRegisterAST2

calculatorSetRegisterAST3 = SetReg "Reg2" calculatorRegisterAST3

calculatorSetRegisterAST4 = SetReg "Reg1" calculatorRegisterAST4

eval :: CalcExprAST -> Store -> Integer
eval (Lit x) _ = x
eval (Add x y) s = eval x s + eval y s
eval (Mult x y) s = eval x s * eval y s
eval (Sub x y) s = eval x s - eval y s
eval (Neg x) s = negate (eval x s)
eval (Reg x) s = getStore s (getRegisterIndex x)

-- Setting a register to a value
execute :: CalcStmtAST -> Store -> Store
execute (SetReg r e) s = setStore (getRegisterIndex r) (eval e s) s
execute (AddReg r e) s = addStore s (getRegisterIndex r) e

test :: IO ()
test = do
  -- Initializing the stores
  let store = setStore (getRegisterIndex "Reg4") 43 (setStore (getRegisterIndex "Reg2") 21 (setStore (getRegisterIndex "Reg1") 11 registerStore))
      store1 = execute (SetReg "Reg1" (Lit 11)) store
      store2 = execute (SetReg "Reg2" (Add (Reg "Reg1") (Lit 10))) store1
      store3 = execute (SetReg "Reg4" (Sub (Mult (Reg "Reg2") (Lit 2)) (Lit (-1)))) store2
      expected = [4, 57, 54, 21]
      evals = map (`eval` store3) [calculatorRegisterAST1, calculatorRegisterAST2, calculatorRegisterAST3, calculatorRegisterAST4]
      correct = all ((== True) . uncurry (==)) (zip expected evals)

  if correct
    && (store == store3)
    && (57 == eval (Reg "Reg1") (execute calculatorSetRegisterAST2 registerStore))
    && (0 == eval (Reg "Reg2") (execute calculatorSetRegisterAST3 registerStore))
    && (0 == eval (Reg "Reg1") (execute calculatorSetRegisterAST4 registerStore))
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
              case stmt of
                (SetReg reg expr) -> do
                  outputStrLn $ show reg ++ " = " ++ show (eval expr state)
                  loop $ execute (SetReg reg expr) state
                (AddReg reg expr) -> do
                  let state' = addStore state (getRegisterIndex reg) expr
                  outputStrLn $ show (Map.toList state')
                  outputStrLn $ show reg ++ " = " ++ show (eval expr state)
                  loop $ execute (AddReg reg expr) state'

-- let SetReg reg expr = stmt
