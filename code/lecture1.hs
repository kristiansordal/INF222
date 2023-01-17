f :: (Integer, Integer) -> Integer
f (x,y) = x * y + x + y

data Natural = Zero | Succ Natural

count :: Natural -> Int
count Zero = 0
count (Succ n) = 1 + count n
