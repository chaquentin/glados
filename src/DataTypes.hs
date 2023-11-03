-- |
--  Module      : DataTypes
--  Description : The abstract syntax tree for the language.
--
-- @
-- program          : statements
--
-- statements       : statement
--                 | statements statement
--
-- statement        : var_declaration
--                 | assignment
--                 | function_declaration
--                 | if_statement
--                 | return_statement
--                 | expression
--
-- var_declaration  : "var" ID "=" expression ";"
--
-- assignment      : ID "=" expression ";"
--
-- function_declaration : "function" ID "(" parameters ")" "{" statements "}"
--
-- parameters      : ID
--                 | ID "," parameters
--
-- if_statement    : "if" expression "then" statements "end"
--                | "if" expression "then" statements "else" statements "end"
--
-- return_statement : "return" expression ";"
--
-- expression      : literal
--                | ID
--                | function_call
--                | expression binary_operator expression
--                | unary_operator expression
--
-- function_call  : ID "(" arguments ")"
--
-- arguments       : expression
--                | expression "," arguments
--
-- binary_operator : "+" | "-" | "*" | "/" | "%" | "==" | "!=" | "<" | ">" | "<=" | ">=" | "and" | "or"
--
-- unary_operator  : "-" | "not"
--
-- literal         : NUMBER
--                | STRING
--                | "true"
--                | "false"
--                | "nil"
--
-- ID              : [a-zA-Z_][a-zA-Z0-9_]*
--
-- NUMBER          : [0-9]+ ("." [0-9]*)?
--
-- STRING          : "\"" [^"]* "\""
-- @
module DataTypes
  ( Ast (..),
    Literal (..),
    BinaryOperator (..),
    UnaryOperator (..),
  )
where

-- | The abstract syntax tree for the language.
data Ast
  = Literal Literal
  | Identifier String
  | FunctionDeclaration String [String] [Ast]
  | VarDeclaration String Ast
  | Assignment String Ast
  | IfStatement Ast [Ast]
  | ReturnStatement Ast
  | FunctionCall String [Ast]
  | BinaryExpression BinaryOperator Ast Ast
  | UnaryExpression UnaryOperator Ast
  deriving (Show, Eq)

-- | The literal values in the language.
data Literal
  = Number Double
  | String String
  | Boolean Bool
  | Nil
  deriving (Show, Eq)

-- | The binary operators in the language.
data BinaryOperator
  = Add
  | Subtract
  | Multiply
  | Divide
  | Modulo
  | Equal
  | NotEqual
  | LessThan
  | GreaterThan
  | LessThanOrEqual
  | GreaterThanOrEqual
  | And
  | Or
  deriving (Show, Eq)

-- | The unary operators in the language.
data UnaryOperator
  = Negate
  | Not
  deriving (Show, Eq)