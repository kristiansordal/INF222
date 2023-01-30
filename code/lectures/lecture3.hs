module Lecture3 where

f :: (Integer, Integer) -> Integer
f (x, y) = x * y + x + y

data Natural = Zero | Succ Natural
  deriving (Show, Eq)

natToInt Zero = 0
natToInt (Succ n) = 1 + natToInt n

count :: Natural -> Int
count Zero = 0
count (Succ n) = 1 + count n

plus :: Natural -> Natural -> Natural
plus Zero n = n
plus (Succ m) n = Succ (plus m n)

mult :: Natural -> Natural -> Natural
mult Zero n = Zero
mult (Succ m) n = n `plus` mult m n

checkPlusCommutative (n, m) = n `plus` m == m `plus` n

checkMultCommutative n m = n `mult` m == m `mult` n

-- intToNat :: (Integral t, Show t) t => Integer -> Natural
-- intToNat 0 = Zero
-- intToNat n = Succ (intToNat n - 1)

testNaturals = do
  print $ Succ Zero
  print (Succ Zero)
  print $ Succ $ Succ $ Succ Zero
  print (Succ (Succ (Succ Zero)))
  let plist = zip [1 .. 10] [1 .. 10]

  print "end"
