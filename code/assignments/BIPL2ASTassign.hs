{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Move brackets to avoid $" #-}
{-# HLINT ignore "Redundant $" #-}

-- | Basic Imperative Programming Language (BIPL) AST:
-- expressions and statements, separating declaration/assignment.
--
-- Also provides an environment for keeping track of variables.
--
-- Magne Haveraen 2023-02-01
module BIPL2ASTassign where

-------------------------

-- | BTL for counting, calculations, choice and variables.
data Expr
  = Z
  | I Expr
  | Plus Expr Expr
  | Mult Expr Expr
  | Minus Expr Expr
  | T
  | F
  | And Expr Expr
  | Or Expr Expr
  | Not Expr
  | Le Expr Expr
  | Ifte Expr Expr Expr
  | Var VarName
  deriving (Show, Eq, Read)

-- | Statements for writing algorithms.
data Stmt
  = Decl VarName Expr -- declaration and initialisation of a variable
  | Assign VarName Expr -- assignment to an already deaclared variable
  | While Expr Stmt
  | IfStmt Expr Stmt Stmt
  | Sequence [Stmt]
  deriving (Show, Eq, Read)

-- | The variables are named by a string.
type VarName = String

-------------------------

-------------------------

-- | An environment is used to keep track of name-attribute values.

-- | An environment is an association list mapping names to values.
-- Here the choice of value type is postponed.
type Environment value = [(Name, value)]

-- | A name is a string.
type Name = String

-- | Find a specific variable name in the environment:
-- home grown version traversing the association list.
find :: Name -> Environment value -> value
find vname ((n, v) : env') =
  if vname == n then v else find vname env'
find vname [] = error $ "Name not found: " ++ (show vname)

-- | Find a specific variable name in the environment: lookup and Maybe monad.
find' :: Name -> Environment value -> value
find' vname env = case lookup vname env of
  Just v -> v
  Nothing -> error $ "Name not found: " ++ (show vname)

-- | Update the name association in the second list with
-- the corrresponding named value from the first list.
-- This corresponds to removing all local declarations from an inner scope (env1),
-- and updating the value association in the outer scope (env2).
updateEnv :: Environment value -> Environment value -> Environment value
updateEnv env1 ((vname, val) : env2) = env'
  where
    env' =
      ( case lookup vname env1 of
          Nothing -> (vname, val)
          Just val' -> (vname, val')
      )
        : updateEnv env1 env2
updateEnv env1 [] = []

-------------------------

-------------------------

-- | Tests for the interpreter on exprList and declaxy
-- - Function vt maps from integers to the value domain
-- - Function eval evaluates expressions (interpreter)
-- - Function exec executes statements (interpreter)
test_evaluations_exec vt eval exec = do
  -- Evaluate exprList with eval env for the integers.
  let env = [("a", vt 7), ("x", vt 2), ("y", vt 5)]
  let resInt = map (eval env) exprList
  print $ "Evaluated test expressions with eval env: " ++ (show $ resInt)
  let env' = exec [] decla ++ exec (exec [] decly) declx
  print $ "Executing declarations with exec " ++ if env == env' then "is OK" else "FAILED!"
  print "end"

-- | A list of expressions (test cases).
exprList :: [Expr]
exprList = [expr0, expr1, expr2, expr3, expr4, expr5, expr6, expr7, expr_sqr_a, expr_f_x_y]

-- Some expressions
expr0 = Z

expr1 = I $ I $ I $ Z

expr2 = Plus (Mult (I $ I $ Z) (I $ I $ I $ Z)) (I Z)

expr3 = Minus expr2 expr1

expr4 = Mult expr1 (Plus expr2 expr3)

expr5 = Plus (Mult expr0 expr1) (Minus expr2 expr4)

expr6 = Minus (I $ Mult expr4 expr4) (Plus expr4 expr4)

expr7 = Ifte (Le expr1 expr3) expr5 expr6

-- | Computing the square of variable "a"
expr_sqr_a = Mult (Var "a") (Var "a")

-- | Computing the the function f(x,y) = x*y + x + y
expr_f_x_y = Plus (Mult (Var "x") (Var "y")) (Plus (Var "x") (Var "y"))

-- | A syntactically problematic term:
-- It is allowed in the metalanguage, but does not make sense.
exprn = Plus T (I $ I $ I $ I $ Le F (I $ I Z))

-- | Declaration statements for variables
-- "a" = 7, "x",2, "y"=5)
decla = Decl "a" expr2

declx = Decl "x" (Minus expr3 (I $ I Z))

decly = Decl "y" (I $ expr3)

declaxy = Sequence [decly, declx, decla]

-- | Stements exposing interesting aspects of the semantics
-- 1: { { y=5; x=2; a=7; } ; res = f(x,y); }
stmtscope1 = Sequence [declaxy, Decl "res" expr_f_x_y]

-- 2: { y=5; x=2; a=7 ; if <= 2 then res1 = a*a else res2 = f(x,y); }
stmtscope2 =
  Sequence
    ( [decly, declx, decla]
        ++ [IfStmt (Le (Var "x") (I $ I Z)) (Assign "res1" expr_sqr_a) (Decl "res2" expr_f_x_y)]
    )

-- 3: { y=5; x=2; a=7 ; while (1 <= x) { res = f(x,y); x = x-1}
stmtscope3 a =
  Sequence
    ( Decl "a" a
        : [ While
              (Le (I Z) (Var "a"))
              (Sequence [Assign "res" expr_sqr_a, Assign "a" (Minus (Var "a") (I Z))])
          ]
    )

stmtscope4 a =
  Sequence
    ( Decl "b" expr_sqr_a
        : Decl "a" a
        : Sequence
          [ declaxy,
            declaxy,
            Sequence [Sequence [Decl "b" expr_sqr_a, declaxy, Decl "c" expr_sqr_a]]
          ]
        : While
          (Le (I Z) (Var "a"))
          (Sequence [Assign "res" expr_sqr_a, Assign "a" (Minus (Var "a") (I Z))])
        : []
    )

stmtscope5 = Sequence [declaxy, declaxy, decly, declx, decla]

-- | Some examples that expose peculiarities of (runtime) scoping rules.
-- - Function exec executes statements (interpreter)
-- - Association list env is a context environment for the statements.
test_scoping_exec exec env = do
  print $ "Some scoping issues"
  print $ "stmtscope1, res=" ++ (show $ lookup "res" (exec env stmtscope1))
  print $ "stmtscope2, res1=" ++ (show $ lookup "res1" (exec env stmtscope2))
  print $ "stmtscope2, res2=" ++ (show $ lookup "res2" (exec env stmtscope2))
  print $ "stmtscope3 0, res=" ++ (show $ lookup "res" (exec env (stmtscope3 Z)))
  print $ "stmtscope3 1, res=" ++ (show $ lookup "res" (exec env (stmtscope3 (I Z))))
  print $ "stmtscope3 2, res=" ++ (show $ lookup "res" (exec env (stmtscope3 (I $ I Z))))
  let env' = if env == [] then env else ("a", snd (head env)) : env
  print $ "stmtscope4 2, res=" ++ (show $ lookup "res" (exec env' (stmtscope4 (I $ I Z))))
  print $ "done"

-----------------------

-- | Procedure for Euclidean division: (q,r) = x `quotRem` y
-- The quotient of x and y is assigned to q while the remainder of x and y is assigned to r.

{-
    procedure eucliddiv ( obs x,y:integer, out q,r:integer );
    begin
    q := 0;
    r := x;
    while y <= r do
        begin
        r := r - y;
        q := q + 1;
        end;
    end;
-}
eucliddiv_stmt :: Stmt
eucliddiv_stmt =
  Sequence
    [ Assign "q" Z,
      Assign "r" (Var "x"),
      While
        (Le (Var "y") (Var "r"))
        ( Sequence
            [ Assign "r" (Minus (Var "r") (Var "y")),
              Assign "q" (Plus (Var "q") (I Z))
            ]
        )
    ]

-- Test cases and oracles for the Euclid algorithm
type XY = (Integer, Integer)

type QR = (Integer, Integer)

type XYQR = (XY, QR)

eucliddiv_problems :: [XY]
eucliddiv_problems = [(13, 7), (49, 7), (16, 32), (32, 16), (32, 15), (0, 9), (1, 9), (8, 9), (9, 9), (10, 9)]

eucliddiv_environments :: Environment value -> (VarName, VarName) -> (Integer -> value) -> XY -> Environment value
eucliddiv_environments env (vx, vy) vt val = (\(x, y) -> env ++ [(vx, vt x), (vy, vt y)]) val

eucliddiv_answers :: [XYQR]
eucliddiv_answers = map (\(x, y) -> ((x, y), quotRem x y)) eucliddiv_problems

-- | Using the eucliddiv_problems test cases to check Euclid's algorithm.
-- - Function vt maps from integers to the value domain
-- - Function exec executes statements (interpreter)
-- - Association list env is a context environment for the algorithm.
test_euclid_exec vt exec env = do
  print $ "Testing Euclid's algorithm for quotient/reminder"
  let xyenvs = map (eucliddiv_environments env ("x", "y") vt) eucliddiv_problems
  let qrvals = map (\(xy, (q, r)) -> (vt q, vt r)) eucliddiv_answers
  let compute = map (\env -> exec env eucliddiv_stmt) xyenvs
  let qrcomputed = map (\env -> (find "q" env, find "r" env)) compute
  print $ if qrvals == qrcomputed then "OK" else "FAILED"
  print $ "end"

--------------------------

-- | Computus

{-
    (** Computing Easter Day for year Y using "Anonymous Gregorian algorithm". *)
    procedure computus ( obs Y:integer; out month,day:integer ) ;
    var a,b,c,d,e,f,g,h,i,k,l,m,n,o:integer;
    begin
    a := Y mod 19;
    b := Y div 100;
    c := Y mod 100;
    d := b div 4;
    e := b mod 4;
    f := (b + 8) div 25;
    g := (b - f + 1) div 3;
    h := (19*a + b - d - g + 15) mod 30;
    i := c div 4;
    k := c mod 4;
    l := (32 + 2*e + 2*i - h - k) mod 7;
    m := (a + 11*h + 22*l) div 451;
    n := (h + l - 7*m + 114) div 31;
    o := (h + l - 7*m + 114) mod 31;
    month := n;
    day := o + 1;
    end ;
-}
computus_2 = I (I Z)

computus_3 = I (I (I Z))

computus_4 = I (I (I (I Z)))

computus_7 = Plus computus_3 computus_4

computus_8 = I computus_7

computus_11 = Plus computus_4 computus_7

computus_15 = Plus computus_7 computus_8

computus_19 = I (I (I (Mult computus_4 computus_4)))

computus_22 = Mult computus_2 computus_11

computus_25 = I (I (I (I (I (I computus_19)))))

computus_30 = I (I (I (I (I computus_25))))

computus_31 = I (computus_30)

computus_32 = Plus computus_7 computus_25

computus_100 = Mult computus_4 computus_25

computus_114 = Minus (Plus computus_15 computus_100) (I Z)

computus_451 = Minus (Mult computus_4 computus_114) (I computus_4)

-- | Computing computus: the Easter day algorithm.
-- Input: "YEAR"; outputs: "month", "day"
computus_stmt :: Stmt
computus_stmt =
  Sequence
    [ -- a := Y mod 19;
      Decl "x" (Var "YEAR"),
      Decl "y" computus_19,
      Decl "q" computus_451,
      Decl "r" computus_451,
      eucliddiv_stmt,
      Decl "a" (Var "r"),
      -- b := Y div 100;
      -- c := Y mod 100;
      Assign "x" (Var "YEAR"),
      Assign "y" computus_100,
      eucliddiv_stmt,
      Decl "b" (Var "q"),
      Decl "c" (Var "r"),
      -- d := b div 4;
      -- e := b mod 4;
      Assign "x" (Var "b"),
      Assign "y" computus_4,
      eucliddiv_stmt,
      Decl "d" (Var "q"),
      Decl "e" (Var "r"),
      -- f := (b + 8) div 25;
      Assign "x" ((Plus (Var "b") computus_8)),
      Assign "y" computus_25,
      eucliddiv_stmt,
      Decl "f" (Var "q"),
      -- g := (b - f + 1) div 3;
      Assign "x" (I (Minus (Var "b") (Var "f"))),
      Assign "y" computus_3,
      eucliddiv_stmt,
      Decl "g" (Var "q"),
      -- h := (19*a + b - d - g + 15) mod 30;
      Assign "x" computus_hexp,
      Assign "y" computus_30,
      eucliddiv_stmt,
      Decl "h" (Var "r"),
      -- i := c div 4;
      -- k := c mod 4;
      Assign "x" (Var "c"),
      Assign "y" computus_4,
      eucliddiv_stmt,
      Decl "i" (Var "q"),
      Decl "k" (Var "r"),
      -- l := (32 + 2*e + 2*i - h - k) mod 7;
      Assign "x" computus_lexp,
      Assign "y" computus_7,
      eucliddiv_stmt,
      Decl "l" (Var "r"),
      -- m := (a + 11*h + 22*l) div 451;
      Assign "x" (Plus (Plus (Var "a") (Mult computus_11 (Var "h"))) (Mult computus_22 (Var "l"))),
      Assign "y" computus_451,
      eucliddiv_stmt,
      Decl "m" (Var "q"),
      -- n := (h + l - 7*m + 114) div 31;
      -- o := (h + l - 7*m + 114) mod 31;
      Assign "x" (Plus (Minus (Plus (Var "h") (Var "l")) (Mult computus_7 (Var "m"))) computus_114),
      Assign "y" computus_31,
      eucliddiv_stmt,
      Decl "n" (Var "q"),
      Decl "o" (Var "r"),
      -- month := n;
      -- day := o + 1;
      Assign "month" (Var "n"),
      Assign "day" (I (Var "o"))
    ]

-- h := (19*a + b - d - g + 15) mod 30;
computus_hexp :: Expr
computus_hexp = (Plus (Minus (Minus (Plus (Mult computus_19 (Var "a")) (Var "b")) (Var "d")) (Var "g")) computus_15)

-- l := (32 + 2*e + 2*i - h - k) mod 7;
computus_lexp :: Expr
computus_lexp = (Minus (Minus (Plus (Plus computus_32 (Mult computus_2 (Var "e"))) (Mult computus_2 (Var "i"))) (Var "h")) (Var "k"))

type Date = (Integer, (Integer, Integer)) -- (Year,(Month,Day))

computus_answers :: [Date]
computus_answers = [(2023, (4, 9)), (2022, (4, 17)), (2021, (4, 4)), (2020, (4, 12)), (2019, (4, 21)), (2011, (4, 24)), (2008, (3, 23)), (2038, (4, 25))]

-- | Using the computus_answers test cases to check the Computus algorithm.
-- - Function vt maps from integers to the value domain
-- - Function exec executes statements (interpreter)
-- - Association list env is a context environment for the algorithm.
test_computus_exec vt exec = do
  print $ "Testing the Computus algorithm for finding Easter Sunday"
  let years = map (\ans -> [("YEAR", vt (fst ans)), ("month", vt 0), ("day", vt 0)]) computus_answers
  let dates = map (\(y, (m, d)) -> (vt y, vt m, vt d)) computus_answers
  let compute = map (\env -> exec env computus_stmt) years
  let computed = map (\env -> (find "YEAR" env, find "month" env, find "day" env)) compute
  -- print $ dates
  -- print $ computed
  print $ if dates == computed then "OK" else "FAILED"
  print $ "end"
