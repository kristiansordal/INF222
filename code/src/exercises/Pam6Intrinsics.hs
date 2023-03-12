-- | A selection of integer function declarations and their semantics.
-- Also instantiates the calculator template for the intrinsics.
--
-- Author Magne Haveraaen
-- Since 2020-03-21
module Pam6Intrinsics where

-- Use signatures

-- Use the calculator template for signatures and function models.
import Pam6CalculatorTemplate
import Pam6Signature

-----------------------

-- | Declaration of operations and their argument list and return type.
integerOperations :: Signature
integerOperations =
  ( ["Integer"],
    [ ("Add", ["Integer", "Integer"], "Integer"),
      ("Mult", ["Integer", "Integer"], "Integer"),
      ("Sub", ["Integer", "Integer"], "Integer"),
      ("Neg", ["Integer"], "Integer"),
      ("Idiv", ["Integer", "Integer"], "Integer"),
      ("Rem", ["Integer", "Integer"], "Integer"),
      ("GCD", ["Integer", "Integer"], "Integer"),
      ("LCM", ["Integer", "Integer"], "Integer")
    ]
  )

-----------------------

-- | Semantics of chosen integer operations.
integerSemantics :: FunModel
integerSemantics "Add" [i1, i2] = i1 + i2
integerSemantics "Mult" [i1, i2] = i1 * i2
integerSemantics "Sub" [i1, i2] = i1 - i2
integerSemantics "Neg" [i] = -i
integerSemantics "Idiv" [i1, 0] =
  error $ "Cannot do integer division of " ++ show i1 ++ " by 0."
integerSemantics "Idiv" [i1, i2] = quot i1 i2
integerSemantics "Rem" [i1, 0] =
  error $ "Cannot do remainder of " ++ show i1 ++ " by 0."
integerSemantics "Rem" [i1, i2] = rem i1 i2
integerSemantics "GCD" [i1, i2] = gcd i1 i2
integerSemantics "LCM" [i1, i2] = lcm i1 i2
integerSemantics fname alist =
  error $ "Unknown function name/arg list " ++ fname ++ show alist

-----------------------

-- | Unit test of the integerOperations:
-- for each declaration in integerOperations check that there is a corresponding integerSemantics.
unittestPam6Intrinsics = do
  print "-- unittestPam6Intrinsics --"
  print $
    -- Expected result of calling the declared functions on relevant argument lists.
    if checkFunModel integerOperations integerSemantics == [21, 110, -1, -10, 0, 10]
      then "Unit tests hold"
      else "Tests failed"

-----------------------

-- | Interactive calculator with variables and given selection of integer operations.

-- | Run the following commands in sequence at the prompt
-- SetVar "Reg4" (Lit 4)
-- SetVar "Reg1" (Fun "Neg" [Fun "Mult" [Fun "Add" [Lit 3,Fun "Sub" [Lit 7,Lit 13]],Lit 19]])
-- SetVar "Reg2" (Fun "Add" [Var "Reg1",Var "Reg4"])
-- AssVar "Reg1" (Var "Reg2")
-- AssVar "Reg1" (Fun "Add" [Var "Reg1",Var "Reg4"])
-- AssVar "Reg1" (Fun "Add" [Var "Reg1",Var "Reg4"])
-- SetVar "v9" (Fun "Add" [Fun "Mult" [Fun "Idiv" [Var "Reg1",Var "Reg4"],Var "Reg4"], Fun "Rem" [Var "Reg1",Var "Reg4"]])
-- show
main = do
  putStrLn "-- Calculator with 6 integer intrinsics --"
  calculatorTemplate integerOperations integerSemantics
