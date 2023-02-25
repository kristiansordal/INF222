-- {-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
-- {-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
-- {-# OPTIONS_GHC -Wno-unused-matches #-}

-- {-# HLINT ignore "Use camelCase" #-}

-- module PIPLcopyinterpreter where

-- -- Import PIPLinterpreter qualified, this is as we need to specify when we want to use functions from this module
-- -- Not importing as qualified will lead to unpredictable behavour during runtime, and will cause the buildArgs function from
-- -- PIPLintepreter (which doesn't have out modified semantics) to be used.

-- import Data.Array
-- import PIPLAST
-- import PIPLexamples
-- import qualified PIPLinterpreter as I hiding (buildArgs, exec, execSeq, perform, run, testcallsemantics)
-- import PIPLstate

-- -- | Build a new state for executing a procedure body:
-- -- Evaluate the procedures arguments in an old environment, creating the new state.
-- -- This is done by pat  tern matching the parameter declaration with the proper actual argument.
-- -- - Obs parameters are by value (copy-in) to a local variable with the parameter's name.
-- -- - Out/Upd parameters are by reference: a local variable with the parameter's name is an alias for the argument variable.
-- buildArgs :: LEnvironment -> [Param] -> [Arg] -> I.State'' -> I.State''
-- buildArgs oldenv (param@(vpname, (Obs, tname)) : params) (arg@(Left vaname) : args) state = state'
--   where
--     val = getValueAlternate vaname I.dataSize oldenv state
--     state' = buildArgs oldenv params args (addVariable vpname val state)
-- buildArgs oldenv (param@(vpname, (Obs, tname)) : params) (arg@(Right e) : args) state = state'
--   where
--     oldstate = replaceEnv oldenv state
--     val = eval oldstate e
--     state' = buildArgs oldenv params args (addVariable vpname val state)
-- buildArgs oldenv (param@(vpname, (Upd, tname)) : params) (arg@(Left vaname) : args) state = state'
--   where
--     val = getValueAlternate vaname I.dataSize oldenv state
--     state' = buildArgs oldenv params args (addVariable vpname val state)
-- buildArgs oldenv (param@(vpname, (Out, tname)) : params) (arg@(Left vaname) : args) state = state'
--   where
--     val = getValueAlternate vaname I.dataSize oldenv state
--     state' = buildArgs oldenv params args (addVariable vpname val state)
-- buildArgs oldenv [] [] state = state
-- buildArgs oldenv params args state = error $ "Argument list mismatch: params=" ++ show params ++ " args=" ++ show args

-- perform :: ProcDefinitions -> Procedure -> [Arg] -> I.State'' -> I.State''
-- perform procdef (Proc procname param stmt) args state = replaceStore state' state
--   where
--     outerenv = getEnv state
--     innerstate = replaceEnv [] state
--     callstate = buildArgs outerenv param args innerstate
--     returnstate = exec procdef callstate stmt
--     state' = replaceEnv outerenv returnstate

--     vals = getVals returnstate param

-- -- where
-- --   getVals (_, _, store) p = map (\x -> x) store

-- getVals (location, env, store) param = filter (\x )

-- -- | Execute the statements and update the state:
-- -- using value (copy-in) semantics for Obs and reference semantics for Upd/Out parameters,
-- -- see the semantic function perform for details.
-- -- Uses static scoping: variables are scoped by the block where they are declared.
-- -- A variable has to be declared/initialised before use, but can be (re)declared many times.
-- -- Procedure declaration are provided as a context to be able to select which procedure to call.
-- exec :: ProcDefinitions -> I.State'' -> Stmt -> I.State''
-- exec procdef state (Decl vname e1) = addVariable vname (eval state e1) state
-- exec procdef state (Assign vname e1) =
--   if isVariable vname state
--     then changeValue vname (eval state e1) state
--     else error $ "Variable has not been declared, vname=" ++ show vname
-- exec procdef state (While e stmt) = replaceStore state' state
--   where
--     [I.VB e'] = eval state e
--     state' =
--       if e'
--         then exec procdef (exec procdef state stmt) (While e stmt)
--         else state
-- exec procdef state (IfStmt e stmt1 stmt2) = replaceStore state' state
--   where
--     [I.VB e'] = eval state e
--     state' =
--       if e'
--         then exec procdef state stmt1
--         else exec procdef state stmt2
-- exec procdef state (Sequence stmts) = replaceStore state' state
--   where
--     state' = execSeq procdef state stmts
-- exec procdef state (Call pname args) = replaceStore state' state
--   where
--     proc = I.findProcedure procdef pname args
--     state' = perform procdef proc args state

-- -- | Execute a list of statements updating the store on the way.
-- execSeq :: ProcDefinitions -> I.State'' -> [Stmt] -> I.State''
-- execSeq procdef state (stmt : stmts) = state''
--   where
--     state' = exec procdef state stmt
--     state'' = execSeq procdef state' stmts
-- execSeq procdef state [] = state

-- -- | Evaluator from expressions to the integer and boolean value domain
-- eval :: I.State'' -> Expr -> [I.ValueDomain'']
-- eval state Z = [I.VI 0]
-- eval state (I e) = [I.VI (1 + e')]
--   where
--     [I.VI e'] = eval state e
-- eval state (Plus e1 e2) = [I.VI (e1' + e2')]
--   where
--     [I.VI e1'] = eval state e1
--     [I.VI e2'] = eval state e2
-- eval state (Mult e1 e2) = [I.VI (e1' * e2')]
--   where
--     [I.VI e1'] = eval state e1
--     [I.VI e2'] = eval state e2
-- eval state (Minus e1 e2) = [I.VI (e1' - e2')]
--   where
--     [I.VI e1'] = eval state e1
--     [I.VI e2'] = eval state e2
-- eval state T = [I.VB True]
-- eval state F = [I.VB False]
-- eval state (And e1 e2) = [I.VB (e1' && e2')]
--   where
--     [I.VB e1'] = eval state e1
--     [I.VB e2'] = eval state e2
-- eval state (Or e1 e2) = [I.VB (e1' || e2')]
--   where
--     [I.VB e1'] = eval state e1
--     [I.VB e2'] = eval state e2
-- eval state (Not e1) = [I.VB (not e1')]
--   where
--     [I.VB e1'] = eval state e1
-- eval state (Le e1 e2) = [I.VB (e1' <= e2')]
--   where
--     [I.VI e1'] = eval state e1
--     [I.VI e2'] = eval state e2
-- eval state (Ifte e0 e1 e2) = if e0' then eval state e1 else eval state e2
--   where
--     [I.VB e0'] = eval state e0
-- eval state (Var vname) = getValue vname I.dataSize state

-- --

-- -- | Run a named procedure from GHC's command line using copy-in/copy-out semantics for the outermost procedure call.
-- -- This code is parameterised by the interpreter and is thus not connected to any specific value domain.
-- -- ProcDefinitions procdef: (list of) procedure definitons.
-- -- ProcName procname: the unique name in procdef of the procedure to be called.
-- -- [value] args: list of inflowing arguments to the procedure (matches Obs/Upd parameters).
-- -- value dval: default value to fill the empty store.
-- -- (ProcDefinitions -> State value -> Stmt -> State value) exec: the interpreter to be used, implicitly deciding the value domain.
-- run :: Show value => ProcDefinitions -> ProcName -> [value] -> value -> (ProcDefinitions -> State value -> Stmt -> State value) -> [value]
-- run procdef procname args dval exec' = outs
--   where
--     -- Create new state with all parameters in fresh variables
--     args' = []
--     proc = I.findProcedure procdef procname args'
--     state = newState I.memorySize dval
--     outs = I.runProc procdef state proc args dval exec'

-- testcallsemantics = do
--   print "Testing swap procedures"
--   print "Copy-in/copy-out semantics"
--   putStr "Swap [34,42]: "
--   print $ run exampleProceduresSwap "swap" [I.VI 34, I.VI 42] (I.VI 12) exec
--   putStr "Swap [42,42]: "
--   print $ run exampleProceduresSwap "swap" [I.VI 42, I.VI 42] (I.VI 12) exec
--   putStr "GroupSwap [34,42]: "
--   print $ run exampleProceduresSwap "groupswap" [I.VI 34, I.VI 42] (I.VI 12) exec
--   putStr "GroupSwap [42,42]: "
--   print $ run exampleProceduresSwap "groupswap" [I.VI 42, I.VI 42] (I.VI 12) exec
--   print "Reference semantics, swapping two variables"
--   putStr "Swap [34,42]: "
--   print $ run exampleProceduresSwap "m_swap2" [I.VI 34, I.VI 42] (I.VI 12) exec
--   putStr "Swap [42,42]: "
--   print $ run exampleProceduresSwap "m_swap2" [I.VI 42, I.VI 42] (I.VI 12) exec
--   putStr "GroupSwap [34,42]: "
--   print $ run exampleProceduresSwap "m_groupswap2" [I.VI 34, I.VI 42] (I.VI 12) exec
--   putStr "GroupSwap [42,42]: "
--   print $ run exampleProceduresSwap "m_groupswap2" [I.VI 42, I.VI 42] (I.VI 12) exec
--   print "Reference semantics, swapping a variable with itself"
--   putStr "Swap [42]: "
--   print $ run exampleProceduresSwap "m_swap1" [I.VI 42] (I.VI 12) exec
--   putStr "GroupSwap [42]: "
--   print $ run exampleProceduresSwap "m_groupswap1" [I.VI 42] (I.VI 12) exec

-- -- TASK 2B - Explanation of the differing results of m_groupswap1
-- -- When running testcallsemantics in PIPLcopyinterpreter, we obtain the following result:
-- --
-- -- GroupSwap [42] : [VI 42]
-- --
-- -- And when running it from the PIPLinterpreter module, we obtain the following result:
-- --
-- -- GroupSwap [42] : [VI 0]
-- --
-- --
-- -- proc_m_groupswap1 =
-- --   Proc
-- --     "m_groupswap1"
-- --     [("a", (Upd, "Integer"))]
-- --     ( Sequence
-- --         [ Call "groupswap" [Left "a", Left "a"]
-- --         ]
-- --     )

-- -- -- | Swapping using group operations (+ -).
proc_groupswap =
  Proc
    "groupswap"
    [("x", (Upd, "Integer")), ("y", (Upd, "Integer"))]
    ( Sequence
        [ Assign "y" (Plus (Var "x") (Var "y")),
          Assign "x" (Minus (Var "y") (Var "x")),
          Assign "y" (Minus (Var "y") (Var "x"))
        ]
    )

-- -- y = 10 + 0
-- -- x = 10 - 10
-- -- y = 10 - 0

-- -- y = 10 + 10
-- -- x = 10 - 10
-- -- y = 10 - 10
-- -- y = 0

-- -- y = x + y
-- -- x = y - x
-- -- y = y - x
