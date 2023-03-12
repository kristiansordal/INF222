{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

-- | Calculator template for variable based integer calculator with explicit signatures.
--
-- Author Magne Haveraaen
-- Since 2020-03-19
module Pam6CalculatorTemplate where

-- Use variable calculator with explicit signatures.

-- Use interpreter with state and explicit signatures.

import Pam4State
import Pam6Signature
import Pam6SignatureAST
import Pam6SignatureInterpreter
-- Use editable console.
import System.Console.Haskeline
-- Use a Haskell parser wrapped in Maybe.
import Text.Read (readMaybe)

-----------------------

-- | Interactive calculator parameterised by function models.
-- First it is checked that the function model accepts the signature.
-- Then for every user provided statement, the calculator checks for defined variables and functions.
calculatorTemplate :: Signature -> FunModel -> IO ()
calculatorTemplate intrinsics@(types, fundecls) funmod = do
  putStrLn "Interactive calulator with variables and the following intrinsic functions"
  -- Check consistency of intrinsic signature
  if null (checkSignature intrinsics)
    then putStr ""
    else
      error $
        "Error: missing type declarations "
          ++ show (checkSignature intrinsics)
          ++ " for intrinsic signature "
          ++ show intrinsics
  -- Validate that all intrinsic functions have a model in funmod
  if length fundecls == length (checkFunModel intrinsics funmod)
    then putStrLn $ signatureToString intrinsics
    else error $ "Not all intrinsic functions have a model, sig=" ++ show intrinsics
  runInputT defaultSettings (loop funmod newState)
  where
    -- Parses and executes CalcStmtAST and prints what happens.
    -- The recursive call to loop must update the store.
    loop :: FunModel -> State -> InputT IO ()
    loop funmod state = do
      input <- getInputLine "¢ "
      case input of
        Nothing -> return ()
        Just "" ->
          do outputStrLn "Finished"; return ()
        Just "show" ->
          do outputStrLn $ "state = " ++ show state; loop funmod state
        Just str -> do
          case readMaybe str :: Maybe CalcStmtAST of
            Nothing -> do
              outputStrLn $ "Not a statement: " ++ str
              loop funmod state
            Just stmt@(SetVar vname expr) | isDeclared vname state -> do
              outputStrLn $ "Error: output variable " ++ show vname ++ " already exists."
              checkExpression state intrinsics expr
              loop funmod state
            Just stmt@(SetVar vname expr)
              | [] <- allDeclared state expr,
                null (typeCheckExpr intrinsics expr) -> do
                  outputStrLn $ "SetVar " ++ show vname ++ " = " ++ show (evaluate funmod state expr)
                  loop funmod $ execute funmod stmt state
            Just stmt@(SetVar vname expr) -> do
              let undeclared = allDeclared state expr
              checkExpression state intrinsics expr
              loop funmod state
            Just stmt@(AssVar vname expr)
              | isDeclared vname state,
                [] <- allDeclared state expr,
                null (typeCheckExpr intrinsics expr) -> do
                  outputStrLn $ "AssVar " ++ show vname ++ " = " ++ show (evaluate funmod state expr)
                  loop funmod $ execute funmod stmt state
            Just stmt@(AssVar vname expr) -> do
              if isDeclared vname state
                then outputStr ""
                else outputStrLn $ "Error: output variable " ++ show vname ++ " has not been declared."
              checkExpression state intrinsics expr
              loop funmod state
    -- \| Check if an expression uses only declared functions and declared variables.
    checkExpression :: State -> Signature -> CalcExprAST -> InputT IO ()
    checkExpression state intrinsics expr = do
      let undeclared = typeCheckExpr intrinsics expr
      if null undeclared
        then outputStr ""
        else outputStrLn $ "Syntax error: expression is not compatible with signature, undeclared functions are " ++ show undeclared
      let undeclared = allDeclared state expr
      if null undeclared
        then outputStr ""
        else outputStrLn $ "Error: expression contains undeclared variables " ++ show undeclared

-----------------------

-- | Pretty print a signature
signatureToString :: Signature -> String
signatureToString sig@(types, fundecls) =
  concatMap
    ( \(fn, args, res) ->
        "  "
          ++ fn
          ++ " :: "
          ++ foldr listargcomma "" args
          ++ " -> "
          ++ res
          ++ "\n"
    )
    fundecls

-- | Insert a comma between two nonempty strings.
listargcomma "" "" = ""
listargcomma str1 "" = str1
listargcomma "" str2 = str2
listargcomma str1 str2 = str1 ++ ", " ++ str2
