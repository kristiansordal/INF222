{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

-- | The notion of a signature and function model.
-- This provides a setting for an open ended set of intrinsic (built in) functions.
--
-- Author Magne Haveraaen
-- Since 2020-03-19
module Pam6Signature where

-----------------------

-- | A signature is a list of type and function declarations.
-- Each type used in a function declaration must be a declared type.
type Signature = ([TypeDeclaration], [FunDeclaration])

-- | A function declaration provides a function name, a list of parameter types, and a return type.
type FunDeclaration = (FunName, [TypeName], TypeName)

-- | A type declaration provides a type name and a documentation string.
-- The documentation string can have embedded newlines.
type TypeDeclaration = TypeName

-- | The model for a function call is a mapping
-- from the function name (String) and related argument list of integers
-- to a resulting integer.
type FunModel = FunName -> [Integer] -> Integer

-- | Type environment: combines a variable with its type.
type TypeEnvironment = [(VarName, TypeName)]

-----------------------

-- | Differentiating between the different purposes for strings

-- | Function names
type FunName = String

-- | Variable names
type VarName = String

-- | Type names
type TypeName = String

-----------------------

-- | Checks that each used type in a signature is a declared type.
-- Returns a list of all undeclared type names.
checkSignature :: Signature -> [TypeName]
checkSignature (types, (fname, params, res) : fundecls) =
  checkTypeLists types (res : params) ++ checkSignature (types, fundecls)
checkSignature (types, []) = []

-- | Checks whether the second list of type names is contained in the first.
-- Returns a list of all misspelled type names from the second list.
checkTypeLists :: [TypeName] -> [TypeName] -> [TypeName]
checkTypeLists types (typ : typs) =
  if typ `elem` types then problems else typ : problems
  where
    problems = checkTypeLists types typs
checkTypeLists types [] = []

-- | Checks if all declared variables have a known type in the signature.
-- Returns a list of all misspelled type names.
checkVarTypes :: Signature -> TypeEnvironment -> [TypeName]
checkVarTypes sig@(types, fdecls) ((var, typ) : tenv) =
  if typ `elem` types then problems else typ : problems
  where
    problems = checkVarTypes sig tenv
checkVarTypes sig [] = []

-- | Checks that a function model covers all cases for a signature.
-- Turns each function declaration in the signature into a call of the corresponding function,
-- in order to check the function model recognises a function and computes a result.
-- Uses a test data set of integers (avoiding 0) corresponding to the declared parameter list.
checkFunModel :: Signature -> FunModel -> [Integer]
checkFunModel (types, (fname, params, res) : fundecls) funmodel =
  funmodel fname [10 .. 9 + toInteger (length params)]
    : checkFunModel (types, fundecls) funmodel
checkFunModel (types, []) funmodel = []

-----------------------

-- | Unit test for consistency checking signatures, variable environments and function models:
-- checkSignature, checkFunModel and checkVarTypes.
unittestPam6Signature = do
  print "-- unittestPam6Signature --"
  let sig1 = ([], [("f", ["X", "Y", "Z"], "Int")]) :: Signature
  let sig2 = (["Integer"], [("f", ["Integer", "Integer"], "Integer")]) :: Signature
  let sig3 = (["X", "Z", "T"], snd sig1)
  let vars = [("x", "Integer"), ("y", "Integer")] :: TypeEnvironment
  let fmod "f" [x, y] = x + y; fmod "f" [x, y, z] = x * y * z
  let ch1 = checkSignature sig1 == ["Int", "X", "Y", "Z"]
  let ch2 = null (checkSignature sig2)
  let ch3 = checkSignature sig3 == ["Int", "Y"]
  let chv = null (checkVarTypes sig2 vars)
  let chfm1 = checkFunModel sig1 fmod == [1320]
  let chfm2 = checkFunModel sig2 fmod == [21]
  print $ if ch1 && ch2 && ch3 && chv && chfm1 && chfm2 then "OK" else "Not OK"
