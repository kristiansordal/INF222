-- | Semantics for register based integer calculator.
-- The values of the registers are stored in a Store.
--
-- Author Magne Haveraaen
-- Since 2020-03-14
module Pam2RegisterStore where

-- Use Haskell's array data structure
import Data.Array

-----------------------

-- | A Store for a register calculator is an array with 10 integer elements.
-- The access functions getregister/setregister need to translate between register and array index.
type Store = Array Integer Integer

-- | Defines a store for 10 registers, all initialised to 0.
registerStore :: Store
registerStore = array (0, 9) [(i, 0) | i <- [0 .. 9]]

-- | Get the value stored for the given register.
getStore :: Store -> Integer -> Integer
getStore store ind =
  if 0 <= ind && ind < 10
    then store ! ind
    else error $ "Not a register index " ++ show ind

-- | Set the value stored for the given register.
setStore :: Integer -> Integer -> Store -> Store
setStore ind val store =
  if 0 <= ind && ind < 10
    then store // [(ind, val)]
    else error $ "Not a register index " ++ show ind ++ " for " ++ show val
