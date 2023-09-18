module DataTypes () where

data Expression
  = Null
  | Number Double
  | Boolean Bool
  | Variable String
  | Pair Expression Expression
  | Exception String
  | Lambda [Expression] Expression
  | PrimitiveProcedure ([Expression] -> Expression)
  | Application Expression [Expression]
  | Definition Expression Expression
  | If Expression Expression Expression
  | Cond [(Expression, Expression)]

instance Show Expression where
  show :: Expression -> String
  show = showExpression

showExpression :: Expression -> String
showExpression Null = "null"
showExpression (Number n) = show n
showExpression (Boolean True) = "#t"
showExpression (Boolean False) = "#f"
showExpression (Variable v) = v
showExpression (Pair e1 e2) = "(" ++ show e1 ++ " . " ++ show e2 ++ ")"
showExpression (Exception s) = "Exception: " ++ s
showExpression (Lambda args body) = "(lambda (" ++ showArgs args ++ ") " ++ show body ++ ")"
showExpression (PrimitiveProcedure _) = "<primitive>"
showExpression (Application e es) = "(" ++ show e ++ " " ++ showArgs es ++ ")"
showExpression (Definition e1 e2) = "(define " ++ show e1 ++ " " ++ show e2 ++ ")"
showExpression (If e1 e2 e3) = "(if " ++ show e1 ++ " " ++ show e2 ++ " " ++ show e3 ++ ")"
showExpression (Cond clauses) = "(cond " ++ showClauses clauses ++ ")"

showArgs :: [Expression] -> String
showArgs = unwords . map show

showClauses :: [(Expression, Expression)] -> String
showClauses = unwords . map showClause

showClause :: (Expression, Expression) -> String
showClause (e1, e2) = "[" ++ show e1 ++ " " ++ show e2 ++ "]"