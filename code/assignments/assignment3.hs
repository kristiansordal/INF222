module Assignment3 where

import BTL2

x = Ifte (Le T (Mult (I Z) (I (I T)))) (Plus T T) (Le (I Z) F)

-- if (T <= (1 * (1 + 1 + T))):
--  T + T
-- else
--  1 <= F

-- Swapped T with (I Z) and F with Z to get an evaluation to actually work
y = Ifte (Le (I Z) (Mult (I Z) (I (I (I Z))))) (Plus (I Z) (I Z)) (Le (I Z) Z)

-- it can make sense i depending on the defined semantics. However
-- Using common sense and logic it doesn't make much sense to multiply
-- a Boolean value with an integer.
