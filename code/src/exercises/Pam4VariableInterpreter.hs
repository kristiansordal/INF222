{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use first" #-}
module Pam4VariableIntepreter where

import Pam4State
import Pam4VariableAST
import System.Console.Haskeline
import Text.Read

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

unittestPam4VariableInterpreter = do
  print "--Unit Test Pam4 Variable Interpreter --"
  let state = addVariable "Reg4" 43 (addVariable "Reg2" 21 (addVariable "Reg1" 11 newState))
      state1 = exec (SetVar "Reg1" (Lit 11)) newState
      state2 = exec (SetVar "Reg2" (Add (Var "Reg1") (Lit 10))) state1
      state3 = exec (SetVar "Reg4" (Sub (Mult (Var "Reg2") (Lit 2)) (Lit (-1)))) state2
  print $
    if eval calculatorVariableAST1 newState == 4
      && eval calculatorVariableAST2 newState == 57
      && eval calculatorVariableAST3 state == 54
      && eval calculatorVariableAST4 state == 21
      && state == state3
      && eval (Var "Reg4") (exec calculatorSetVariableAST1 newState) == 4
      && eval (Var "Reg1") (exec calculatorSetVariableAST2 newState) == 57
      && eval (Var "Reg2") (exec calculatorSetVariableAST3 state3) == 54
      && eval (Var "Reg1") (exec calculatorSetVariableAST4 state3) == 21
      then "Unit Tests Hold"
      else "Tests Failed"

main = do
  putStrLn "-- Interactive calulator with variables --"
  runInputT defaultSettings (loop newState)
  where
    -- Parses and execs CalcStmtAST and prints what happens.
    -- The recursive call to loop must update the store.
    loop :: State -> InputT IO ()
    loop state = do
      input <- getInputLine "¢ "
      case input of
        Nothing -> return ()
        Just "" ->
          do outputStrLn "Finished"; return ()
        Just "show" ->
          do outputStrLn $ "state = " ++ show state; loop state
        Just str -> do
          case readMaybe str :: Maybe CalcStmtAST of
            Nothing -> do
              outputStrLn $ "Not a statement: " ++ str
              loop state
            Just (SetVar vname _) | isDeclared vname state -> do
              outputStrLn $ "Error: Variable " ++ show vname ++ " already exists."
              loop state
            Just stmt@(SetVar vname expr) | allDeclared expr state -> do
              outputStrLn $ "SetVar " ++ show vname ++ " = " ++ show (eval expr state)
              loop $ exec stmt state
            Just (SetVar _ _) -> do
              outputStrLn "Error: not all variables used in the expression have been declared. "
              loop state
            Just stmt@(AssVar vname expr)
              | isDeclared vname state,
                allDeclared expr state -> do
                  outputStrLn $ "AssVar " ++ show vname ++ " = " ++ show (eval expr state)
                  loop $ exec stmt state
            Just (AssVar vname expr) | allDeclared expr state -> do
              outputStrLn $ "Error: Variable " ++ show vname ++ " has not been declared."
              loop state
            Just (AssVar _ _) -> do
              outputStrLn "Error: not all variables used in the expression have been declared. "
              loop state

-- | Helper function that traverses an expression and checks if
-- all variables have been declared.
allDeclared :: CalcExprAST -> State -> Bool
allDeclared (Lit _) _ = True
allDeclared (Add e1 e2) state = allDeclared e1 state && allDeclared e2 state
allDeclared (Mult e1 e2) state = allDeclared e1 state && allDeclared e2 state
allDeclared (Sub e1 e2) state = allDeclared e1 state && allDeclared e2 state
allDeclared (Neg e1) state = allDeclared e1 state
allDeclared (Var vname) state = isDeclared vname state
