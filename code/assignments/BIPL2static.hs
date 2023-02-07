-- | Basic Imperative Programming Language (BIPL) with static scoping.
--
-- Stripped down semantics: ValueDomain'' = VI Integer | VB Bool
--
-- Magne Haveraen 2023-02-01
module BIPL2static where

import BIPL2ASTassign

-------------------------

-------------------------

-- | The semantic domain are the integers and booleans
data ValueDomain'' = VI Integer | VB Bool
  deriving (Show, Eq, Read)

-- | Embedding (converting) an integer to a value domain element.
iembed :: Integer -> ValueDomain''
iembed i = VI i

-- | Embedding (converting) a boolean to a value domain element.
bembed :: Bool -> ValueDomain''
bembed b = VB b

-- | The environment is for value domain ValueDomain''.
type Environment'' = Environment ValueDomain''

-------------------------

-- | The interpreter for static scoping.

-- | Evaluator from BTL to the integer and boolean value domain
eval :: Environment'' -> Expr -> ValueDomain''
eval env Z = VI 0
eval env (I e) = VI (1 + e')
  where
    VI e' = eval env e
eval env (Plus e1 e2) = VI (e1' + e2')
  where
    VI e1' = eval env e1
    VI e2' = eval env e2
eval env (Mult e1 e2) = VI (e1' * e2')
  where
    VI e1' = eval env e1
    VI e2' = eval env e2
eval env (Minus e1 e2) = VI (if e1' <= e2' then 0 else e1' - e2')
  where
    VI e1' = eval env e1
    VI e2' = eval env e2
eval env T = VB True
eval env F = VB False
eval env (And e1 e2) = VB (e1' && e2')
  where
    VB e1' = eval env e1
    VB e2' = eval env e2
eval env (Or e1 e2) = VB (e1' || e2')
  where
    VB e1' = eval env e1
    VB e2' = eval env e2
eval env (Not e1) = VB (not e1')
  where
    VB e1' = eval env e1
eval env (Le e1 e2) = VB (e1' <= e2')
  where
    VI e1' = eval env e1
    VI e2' = eval env e2
eval env (Ifte e0 e1 e2) = if e0' then (eval env e1) else (eval env e2)
  where
    VB e0' = eval env e0
eval env (Var vname) = find vname env

-- eval env _ = error "WRONG"

-------------------------

-- | Execute the statements and update the environment.
-- Uses static scoping: variables are scoped by the block where they are declared.
-- A variable can only be declared (and initalised) once in a block and its subblocks.
-- It has to be declared before being assigned to.
exec :: Environment'' -> Stmt -> Environment''
exec env (Decl vname e1) =
  if lookup vname env == Nothing
    then (vname, eval env e1) : env
    else error $ "Variable already declared, vname=" ++ (show vname)
exec env (Assign vname e1) =
  if lookup vname env /= Nothing
    then (vname, eval env e1) : env
    else error $ "Variable has not been declared, vname=" ++ (show vname)
exec env (While e stmt) = updateEnv env' env
  where
    VB e' = eval env e
    env' =
      if e'
        then exec (exec env stmt) (While e stmt)
        else env
exec env (IfStmt e stmt1 stmt2) = updateEnv env' env
  where
    VB e' = eval env e
    env' =
      if e'
        then exec env stmt1
        else exec env stmt2
exec env (Sequence stmts) = updateEnv env' env
  where
    env' = execSeq env stmts

execSeq env (stmt : stmts) = env''
  where
    env' = exec env stmt
    env'' = execSeq env' stmts
execSeq env [] = env

-------------------------

-- | Tests for the semantic domains
tests = do
  test_evaluations
  test_scoping
  test_euclid
  test_computus

test_evaluations = test_evaluations_exec iembed eval exec

test_scoping = test_scoping_exec exec []

test_scoping_alt =
  test_scoping_exec
    exec
    [("a", VI 42), ("x", VI 31), ("y", VI 20), ("res", VI 9), ("res1", VI $ -2), ("res2", VI $ -13)]

test_euclid = test_euclid_exec iembed exec [("q", VI 42), ("r", VI 42)]

test_computus = test_computus_exec iembed exec
