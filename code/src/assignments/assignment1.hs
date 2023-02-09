-- 1.3
-- 1. What is the most general typing of the following function?

f :: (Num a) => a -> a -> a
f x y = x + y

-- 2. Define several implementations of a function sum_squares that
-- for a given positive integer x, calculates the sum of the areas of
-- all the squares with integer side smaller or equal to x
-- sum_squares :: (Num a) => a -> a
-- a) define this function in the way you find the most obvious
sumSquares x = sum (map (\x' -> x' * x') [1 .. x])

-- b) define this function using a list comprehension
sumSquares' x = sum $ [n * n | n <- [1 .. x]]

-- c) define this function using an anonymous function
-- already did

-- 3. Define a recursive function that finds the indices
--  all the occurences of a character in a string
indexOccurences :: String -> Char -> Int -> [Int]
indexOccurences [] _ _ = []
indexOccurences (x : xs) c y
  | x == c = y : indexOccurences xs c (y + 1)
  | otherwise = indexOccurences xs c (y + 1)

-- Define a higher-order function that takes a function and a list of elements
-- as a parameter, and recursively applies the function to all the elements of a list
customFold :: (Int -> Int -> Int) -> [Int] -> Int
customFold f [] = 0
customFold f (x : xs) = f x 1 + customFold f xs

-- these functions are normally called fold, or reduce.

-- 5. Define the following data type

-- Based on boolean logic, build a function eval :: Expr -> Expr that
-- reduces the Expr passed as a parameter to either T or F
data Expr = T | F | And Expr Expr | Or Expr Expr | Not Expr
  deriving (Show)


-- eval :: Expr -> Expr
-- eval T = T
-- eval F = F
-- eval (And T T) = T
-- eval (And T F) = F
-- eval (And F T) = F
-- eval (And F F) = F
-- eval (And x y) = eval (And (eval x) (eval y))
-- eval (Or T T) = T
-- eval (Or T F) = T
-- eval (Or F T) = T
-- eval (Or F F) = F
-- eval (Or x y) = eval (Or (eval x) (eval y))
-- eval (Not T) = F
-- eval (Not F) = T
--
eval :: Expr -> Expr
eval T = T
eval F = F
eval (And x y) = case (eval x, eval y) of (T, T) -> T
                                        _ -> F
eval (Or x y) = case (eval x, eval y) of (F, F) -> F
                                       _ -> T
eval (Not e) = case eval e of T -> F
                             F -> T

