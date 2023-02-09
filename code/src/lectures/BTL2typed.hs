data IExpr
  = Z
  | I IExpr
  | Plus IExpr
  | Mult IExpr IExpr
  | Minus IExpr IExpr
  | Ifte BExpr IExpr IExpr

data BExpr
  = T
  | F
  | And BExpr BExpr
  | Or BExpr BExpr
  | Not BExpr
