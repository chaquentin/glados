module DataTypes
  ( Ast (..),
  )
where

data Ast
  = Number Double
  | Boolean Bool
  | Variable String
  | Null
  | List [Ast]
  | Lambda [String] Ast
  | Function [String] Ast
  | If Ast Ast Ast
  | Define String Ast
  | BuiltIn ([Ast] -> Ast)

instance Show Ast where
  show (Number n) = show n
  show (Boolean b) = show b
  show (Variable v) = v
  show Null = "null"
  show (List l) = "(" ++ unwords (map show l) ++ ")"
  show (Lambda args body) = "(lambda (" ++ unwords args ++ ") " ++ show body ++ ")"
  show (If c t e) = "(if " ++ show c ++ " " ++ show t ++ " " ++ show e ++ ")"
  show (Define v e) = "(define " ++ v ++ " " ++ show e ++ ")"
  show (BuiltIn _) = "<built-in function>"
  show (Function args body) = "(function (" ++ unwords args ++ ") " ++ show body ++ ")"

instance Eq Ast where
  (Number n) == (Number n') = n == n'
  (Boolean b) == (Boolean b') = b == b'
  (Variable v) == (Variable v') = v == v'
  Null == Null = True
  (List l) == (List l') = l == l'
  (Lambda args body) == (Lambda args' body') = args == args' && body == body'
  (If c t e) == (If c' t' e') = c == c' && t == t' && e == e'
  (Define v e) == (Define v' e') = v == v' && e == e'
  (BuiltIn _) == (BuiltIn _) = True
  (Function args body) == (Function args' body') = args == args' && body == body'
  _ == _ = False
