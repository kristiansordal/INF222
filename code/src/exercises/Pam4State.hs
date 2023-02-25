-- | Semantics for variable based integer calculator.
-- It uses State to keep track of variables and their values.
-- This is separated into a environment which keeps track of variables, and
-- a store which keeps track of their (changing) values.
-- Safety is increased by ensuring that variables have been declared
-- before they can be used.
--
-- Author Magne Haveraaen
-- Since 2020-03-19
module Pam4State where

-- Use Haskell's array data structure
import Data.Array
import Pam4VariableAST

-----------------------

-- | A state is an environment of variable-store index associations, and
-- a store which at each store index keeps a value (for that variable).
type State = (Environment, Store)

-- | A new state is an empty environment with an empty store.
newState :: State
newState = (emptyEnvironment, emptyStore)

-- | Checks if the variable name has been declared.
isDeclared :: String -> State -> Bool
isDeclared vname (env, store) =
  case lookup vname env of
    Just loc -> True
    Nothing -> False

-- | Gets the value linked to the variable in the state.
getValue :: String -> State -> Integer
getValue vname (env, store) =
  case lookup vname env of
    Just loc -> getStore store loc
    Nothing -> error $ "Variable " ++ vname ++ " not found in state " ++ show env

-- | Add a new variable with value to the state.
addVariable :: String -> Integer -> State -> State
addVariable vname value (env, store) = (env', store')
  where
    (high', store') = enlargeStore store value
    env' = declareVariable vname high' env

-- | Changes the value associated with a known variable.
changeValue :: String -> Integer -> State -> State
changeValue vname value (env, store) = (env, store')
  where
    store' = case lookup vname env of
      Just loc -> setStore loc value store
      Nothing -> error $ "Variable " ++ show vname ++ " not found in state " ++ show env

-----------------------

-- | An Environemnt for a calculator with variables.
-- It stores an association list of distinct variable names and their store index.
-- As such, it can be searched by the Haskell standard function
--   lookup :: Eq a => a -> [(a, b)] -> Maybe b
type Environment = [(String, Integer)]

-- | Defines an empty environment
emptyEnvironment :: Environment
emptyEnvironment = []

-- | Add a new variable (and a store index) to the environment.
declareVariable :: String -> Integer -> Environment -> Environment
declareVariable vname ind env =
  case lookup vname env of
    Just _ ->
      error $
        "New variable "
          ++ show (vname, ind)
          ++ " already registered in "
          ++ show env
    Nothing -> (vname, ind) : env

-----------------------

-- | A Store for a calculator is an array where the number of indices
-- corresponds to the number of distinct variables.
type Store = Array Integer Integer

-- | Defines an empty store
emptyStore :: Store
emptyStore = array (0, -1) []

-- | Get the value stored for the given index.
getStore :: Store -> Integer -> Integer
getStore store ind =
  if low <= ind && ind <= high
    then store ! ind
    else error $ "Not a store index " ++ show ind
  where
    (low, high) = bounds store

-- | Set the value stored at the given index.
setStore :: Integer -> Integer -> Store -> Store
setStore ind val store =
  if low <= ind && ind <= high
    then store // [(ind, val)]
    else error $ "Not a store index " ++ show ind ++ " for " ++ show val
  where
    (low, high) = bounds store

-- | Get next store index and increase store size with one and set value at new location.
enlargeStore :: Store -> Integer -> (Integer, Store)
enlargeStore store value = (high', store')
  where
    (low, high) = bounds store
    high' = high + 1
    storelist = assocs store
    store' = array (low, high') (storelist ++ [(high', value)])

-----------------------

-- | Unit tests for State.
unittestPam4State = do
  print "-- unittestPam4State --"
  -- putStrLn \$ "Empty state = " ++ (show newState)
  let state1 = addVariable "v1" 1 newState
  let state2 = addVariable "v2" 4 state1
  let state3 = addVariable "v3" 9 state2
  let state4 = changeValue "v2" 25 state3
  -- putStrLn \$ "Value of v1 == " ++ (show \$ getValue "v1" state4)
  -- putStrLn \$ "Value of v2 == " ++ (show \$ getValue "v2" state4)
  -- putStrLn \$ "Value of v3 == " ++ (show \$ getValue "v3" state4)
  -- putStrLn \$ "State3 = " ++ (show state3)
  putStrLn $
    if 1 == getValue "v1" state4
      && 25 == getValue "v2" state4
      && 9 == getValue "v3" state4
      then "Unit tests hold"
      else "Tests failed"
