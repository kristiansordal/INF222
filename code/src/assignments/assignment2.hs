data SomeType
  = A Bool Bool Bool
  | B Bool
  | C

data Natural = Zero | Succ Natural | Negative Natural
  deriving (Show, Eq, Read)

allTrue (A x y z) = x && y && z
allTrue (B x) = x
allTrue C = False

naturalToInt :: Natural -> Int
naturalToInt (Negative n) = (-1) + naturalToInt n
naturalToInt Zero = 0
naturalToInt (Succ n) = 1 + naturalToInt n
