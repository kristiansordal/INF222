-- | Basic Imperative Programming Language (BIPL) with runtime scoping.
--
-- Stripped down semantics: ValueDomain'' = VI Integer | VB Bool
--
-- Magne Haveraen 2023-02-01
module BIPL2runtime where

import BIPL2ASTassign

-------------------------

-- | The semantic domain are the integers and booleans
data ValueDomain'' = VI Integer | VB Bool
  deriving (Show, Eq, Read)

-- | Embedding (converting) an integer to a value domain element.
iembed :: Integer -> ValueDomain''
iembed = VI

-- | Embedding (converting) a boolean to a value domain element.
bembed :: Bool -> ValueDomain''
bembed = VB

-- | The environment is for value domain ValueDomain''.
type Environment'' = Environment ValueDomain''

-------------------------

-- | The interpreter for runtime scoping.

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
eval env (Ifte e0 e1 e2) = if e0' then eval env e1 else eval env e2
  where
    VB e0' = eval env e0
eval env (Var vname) = find vname env

-- eval env _ = error "WRONG"

-------------------------

-- | Execute the statements and update the environment.
-- Uses runtime scoping:
-- declarations are processed as seen at runtime.
exec :: Environment'' -> Stmt -> Environment''
exec env (Decl vname e1) = (vname, eval env e1) : env
exec env (Assign vname e1) = (vname, eval env e1) : env
exec env (While e stmt) = env'
  where
    VB e' = eval env e
    env' =
      if e'
        then exec (exec env stmt) (While e stmt)
        else env
exec env (IfStmt e stmt1 stmt2) = env'
  where
    VB e' = eval env e
    env' =
      if e'
        then exec env stmt1
        else exec env stmt2
exec env (Sequence (stmt : stmts)) = env''
  where
    env' = exec env stmt
    env'' = exec env' (Sequence stmts)
exec env (Sequence []) = env

-------------------------

-------------------------

-- | Tests for the semantic domains
tests = do
  -- test_evaluations
  test_scoping

-- test_euclid
-- test_computus

test_evaluations = test_evaluations_exec iembed eval exec

test_scoping = test_scoping_exec exec []

test_euclid = test_euclid_exec iembed exec []

test_computus = test_computus_exec iembed exec
