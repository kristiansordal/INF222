-- | Natural numbers with operations and properties.
--
-- Magne Haveraen 2023-01-23

module Naturals where

-------------------------
-- | Natural numbers.
-- Two cases for counting: Zero and Successor.
data Natural
  = Zero
  | Succ Natural
  deriving (Show,Eq,Read)


-------------------------
-- | Conversions between naturals and integrals.
-- These should be isomorphisms.

-- | Take a natural number and compute its integer value.
natToInt :: Integral p => Natural -> p
natToInt Zero = 0
natToInt (Succ n) = 1 + natToInt n

-- | Take an integer and create a natural number.
intToNat :: (Integral t, Show t) => t -> Natural
intToNat 0 = Zero
intToNat n | n > 0 = Succ (intToNat (n-1))
intToNat n = error $ "Cannot convert to naturals: n=" ++ (show n)

-- | Check that first calling natToInt then intToNat returns the argument natural number.
check_natToInt_intToNat :: Natural -> Bool
check_natToInt_intToNat n = n == intToNat (natToInt n)

-- | Check that first calling intToNat then natToInt returns the argument number.
check_intToNat_natToInt :: (Integral t, Show t) => t -> Bool
check_intToNat_natToInt n = n == natToInt (intToNat n)


-------------------------
-- | Operations on naturals: addition, multiplication, comparison, and friends.

-- | The constant zero
zero :: Natural
zero = Zero
-- | The constant one
one :: Natural
one = Succ Zero
-- | Predecessor: reducing by one, stopping at zero
pred :: Natural -> Natural
pred Zero = Zero
pred (Succ n) = n

-- | Adding two naturals
plus :: Natural -> Natural -> Natural
plus n Zero = n
plus n (Succ i) = Succ $ plus n i

-- | Multiplying two naturals
mult :: Natural -> Natural -> Natural
mult n Zero = Zero
mult n (Succ i) = plus n $ mult n i

-- | Subtracting two naturals, yield 0 if second natural is two large
monus :: Natural -> Natural -> Natural
monus n Zero = n
monus (Succ n) (Succ i) = monus n i
monus Zero i = Zero

-- | Compare two naturals: whether the first is less than or equal to the second
le :: Natural -> Natural -> Bool
le Zero i = True
le (Succ n) (Succ i) = le n i
le (Succ n) Zero = False


-------------------------
-- | Checks for the operations on naturals

-- | We can swap the arguments to addition
check_plus_commutative :: (Natural, Natural) -> Bool
check_plus_commutative (n,m) = plus n m == plus m n

-- | We can regroup the arguments to addition
check_plus_associative :: (Natural, Natural, Natural) -> Bool
check_plus_associative (n,m,p) = n `plus` (m `plus`p) == (n `plus` m) `plus`p

-- | We can swap the arguments to multiplication
check_mult_commutative :: (Natural, Natural) -> Bool
check_mult_commutative (n,m) = mult n m == mult m n

-- | We can regroup the arguments to multiplication
check_mult_associative :: (Natural, Natural, Natural) -> Bool
check_mult_associative (n,m,p) = n `mult` (m `mult`p) == (n `mult` m) `mult`p

-- | We can regroup multiplication and addition: n * (m+p) = n*m + n*p
check_distributive :: (Natural, Natural, Natural) -> Bool
check_distributive (n,m,p) = n `mult` (m `plus`p) == (n `mult` m) `plus` (n `mult` p)

-- | Multiplying with zero gives zero.
check_annihilation :: Natural -> Bool
check_annihilation n = Zero `mult` n == Zero

-- | Adding zero has no effect.
check_plus_zero :: Natural -> Bool
check_plus_zero n = Zero `plus` n == n

-- | Multiplying with one has no effect.
check_mult_one :: Natural -> Bool
check_mult_one n = one `mult` n == n


-- | Subtracting a larger number gives zero.
check_monus :: (Natural, Natural) -> Bool
check_monus (n,m) | n `le` m = monus n m == Zero
check_monus (n,m) = True

-- | Subtraction undoes addition.
check_monus_inverse :: (Natural, Natural) -> Bool
check_monus_inverse (n,m) = monus (n `plus` m) m == n


-- | Comparing a natural to itelf
check_reflexive :: Natural -> Bool
check_reflexive n = n `le` n

-- | Antisymmetri: If n <= m and m <= n, then they are equal
check_antisymmetry :: (Natural, Natural) -> Bool
check_antisymmetry (n,m) = 
  if (n `le` m) && (m `le` n) then n == m else True

-- | Transitive: If n <= m and m <= p, then also n <= p
check_transitive :: (Natural, Natural, Natural) -> Bool
check_transitive (n,m,p) =
  if (n `le` m) && (m `le` p) then (n `le` p) else True

-- | All naturals compare to each other one way or the other
check_connected :: (Natural, Natural) -> Bool
check_connected (n,m) = (n `le` m) || (m `le` n)




-- | Ordering and numerical operations behave well together
check_ordered_succ :: Natural -> Bool
check_ordered_succ n = n `le` (Succ n)


-------------------------
-- | Collating the checks above into standard properties.
-- | These are formulated to carry out the checks for lists of test cases:
-- list of naturals, list of pairs of naturals and list of triples of naturals.

-- | Checking isomorphism property of the conversion functions.
check_conversion_props :: (Foldable t1, Integral t2, Show t2) => t1 Natural -> [t2] -> Bool
check_conversion_props data1 dataInt 
  =  all check_natToInt_intToNat data1 
  && all check_intToNat_natToInt ([0,1,2]++dataInt)

-- | Checking the commutative semiring properties of the naturals.
check_semiring_props :: (Foldable t1, Foldable t2, Foldable t3) => t3 Natural -> t1 (Natural, Natural) -> t2 (Natural, Natural, Natural) -> Bool
check_semiring_props data1 data2 data3 
  =  all check_plus_commutative data2 
  && all check_plus_associative data3
  && all check_mult_commutative data2
  && all check_mult_associative data3
  && all check_distributive data3
  && all check_annihilation data1
  && all check_plus_zero data1
  && all check_mult_one data1

-- | Checking the monus properties.
check_monus_props :: Foldable t => p1 -> t (Natural, Natural) -> p2 -> Bool
check_monus_props data1 data2 data3 
  =  all check_monus data2 
  && all check_monus_inverse data2

-- | Checking the total order properties
check_totalOrder_props :: (Foldable t1, Foldable t2, Foldable t3) => t1 Natural -> t2 (Natural, Natural) -> t3 (Natural, Natural, Natural) -> Bool
check_totalOrder_props data1 data2 data3
  =  all check_reflexive data1
  && all check_antisymmetry data2
  && all check_transitive data3
  && all check_connected data2

-- | Checking that the total order is compatible with the successor function.
check_ordered_counting_props :: Foldable t => t Natural -> p1 -> p2 -> Bool
check_ordered_counting_props data1 data2 data3 
  = all check_ordered_succ data1


-------------------------
-- | Test program for natural numbers:
-- | The semiring properties, the total order properties, and some more.
-- NB! This will take considerable time due to the creation of large naturals.
test_naturals :: IO ()
test_naturals = do
  -- Define the test data sets: all combinations of 0, 1, 2.
  let dataBasic1 = [ zero, one, Succ one]
  let dataBasic2 = [ (x,y) | x<-dataBasic1, y<-dataBasic1 ]
  let dataBasic3 = [ (x,y,z) | x<-dataBasic1, y<-dataBasic1, z<-dataBasic1 ]
  -- Define the test data sets: some random numbers and their rotations.
  let dataInt = [3,4,5,6,7,8,9,12,13,23,24,26,29,35,43,45,54,64,255]
  let lenData = length dataInt
  let datar0 = map intToNat dataInt
  let datar1 = drop (div lenData 5) datar0 ++ take (div lenData 5) datar0
  let datar2 = drop (div lenData 3) datar0 ++ take (div lenData 3) datar0
  -- Actual test data: basic data sets combined with zipped rotated sets.
  let data1 = dataBasic1 ++ datar0
  let data2 = dataBasic2 ++ zip datar0 datar1
  let data3 = dataBasic3 ++ zip3 datar0 datar1 datar2
  -- print $ map natToInt data1
  -- print $ map (\(x,y)->(natToInt x, natToInt y)) data2
  -- print $ map (\(x,y,z)->(natToInt x, natToInt y, natToInt z)) data3
  --
  -- Test integer/natural conversions
  let conversionsCheck = check_conversion_props data1 ([0,1,2]++dataInt)
  print $ if conversionsCheck then "Conversions are OK" else "ERROR: The conversions are NOT isomorphisms"
  -- Test commutative semiring properties
  let semiringCheck = check_semiring_props data1 data2 data3
  print $ if semiringCheck then "The naturals are a semiring" else "ERROR: The naturals are NOT a semiring"
  -- Test total order properties
  let totalOrderCheck = check_totalOrder_props data1 data2 data3
  print $ if totalOrderCheck then "The naturals are a total order" else "ERROR: The naturals are NOT a total order"
  -- Test monus properties
  let monusCheck = check_monus_props data1 data2 data3
  print $ if monusCheck then "The monus function is OK" else "ERROR: The monus function is NOT OK"
  -- Test ordered semiring properties
  let orderedSemiringCheck = check_ordered_counting_props data1 data2 data3
  print $ if orderedSemiringCheck then "The naturals are ordered by successor function" else "ERROR: The naturals' ordering are NOT according to the successor function"
  print $ "end"

