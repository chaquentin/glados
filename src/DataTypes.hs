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
  | If Ast Ast Ast
  | Define String Ast
  deriving (Show, Eq)
