module Lecture6 where

import Data.Array
import Data.Word

-- data Stmt
--   = Decl VarName Expr
--   | Assign VarName Expr
--   | While Expr Stmt
--   | Ifstmt Expr Stmt Stmt
--   | Sequence [Stmt]
--
type Environment word16 = [(String, word16)]

type Envword16 = Environment Word16

type Store value = Array Location value

type Location = Word8

type LocEnv = Environment Location

type State value = (LocEnv, Store value)

eval :: State value -> Expr -> value
