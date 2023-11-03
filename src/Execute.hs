-- |
-- Module      : Execute
-- Description : The evaluator for the language.
module Execute
  ( trancode,
    Program,
    Value (..),
    Instruction (..),
  )
where

import qualified DataTypes as DT

-- data Value :
-- Number      Int                 -- an integer
-- Chaine      String              -- a string
-- Boolean     Bool                -- True or False
-- Builtin     Builtin             -- a builtin function
-- Function    String  Int Program -- a function with a name, a number of arguments and a program

-- data Builtin :
-- Add     -- addition
-- Sub     -- subtraction
-- Mul     -- multiplication
-- Div     -- division
-- Eqq     -- equality
-- Less    -- less than
-- Not     -- not

-- Instruction :
-- Push Value              -- push a value into the stack
-- Call                    -- call a function
-- JumpIfFalse Int         -- jump a Number of instructions if the top of the stack is False
-- PushArg Int             -- push the nth argument into the stack
-- PushEnv String          -- push the Program since the environment into the stack
-- PushVar String          -- push the value of a variable into the stack
-- ChangeVar String Value  -- change the value of a variable if the variable doesn't exist, create it
-- Ret                     -- return from a function

-- type Stack = [Value]                    -- the stack
-- type Program = [Instruction]            -- the program
-- type Args = [Value]                     -- the arguments
-- type Env = [(String, (Int, Program))]   -- the environment
-- type Vars = [(String, Value)]           -- the variables

data Value
  = Number Int
  | Chaine String
  | Boolean Bool
  | Builtin Builtin
  | Function String Int Program

instance Show Value where
  show (Number n) = show n
  show (Chaine s) = show s
  show (Boolean b) = show b
  show (Builtin b) = show b
  show (Function name args _) = name ++ " " ++ show args ++ " :"

data Builtin
  = Add
  | Sub
  | Mul
  | Div
  | Eqq
  | Less
  | Not
  deriving (Show)

data Instruction
  = Push Value
  | Call
  | JumpIfFalse Int
  | PushArg Int
  | PushEnv String
  | PushVar String
  | ChangeVar String Value
  | Ret
  deriving (Show)

-- | The stack
type Program = [Instruction]

doublToInteger :: Double -> Int
doublToInteger = round

transcodeIf :: DT.Ast -> Program
transcodeIf ast = case ast of
  DT.IfStatement cond then' ->
    let transcodeThen = transcodeStatements then'
        thenLength = length transcodeThen
     in trancode cond
          ++ [JumpIfFalse (thenLength + 2)]
          ++ transcodeThen
  _ -> error "Not an if statement"

transcodeStatements :: [DT.Ast] -> Program
transcodeStatements = concatMap trancode

trancode :: DT.Ast -> Program
trancode ast = case ast of
  DT.Literal (DT.Number n) -> [Push (Number (doublToInteger n))]
  DT.Literal (DT.String s) -> [Push (Chaine s)]
  DT.Literal (DT.Boolean b) -> [Push (Boolean b)]
  DT.Literal DT.Nil -> [Push (Boolean False)]
  DT.Identifier i -> [PushVar i]
  DT.IfStatement {} -> transcodeIf ast
  DT.FunctionDeclaration f args body -> [Push (Function f (length args) (concatMap trancode body))]
  DT.VarDeclaration v a -> trancode a ++ [ChangeVar v (Number 0)]
  DT.Assignment v a -> trancode a ++ [ChangeVar v (Number 0)]
  DT.ReturnStatement a -> trancode a ++ [Ret]
  DT.FunctionCall f args -> concatMap trancode args ++ [PushEnv f, Call]
  DT.BinaryExpression op a b -> trancode a ++ trancode b ++ [Push (Builtin (binaryOperator op))]
  DT.UnaryExpression op a -> trancode a ++ [Push (Builtin (unaryOperator op))]
  where
    binaryOperator op = case op of
      DT.Add -> Add
      DT.Subtract -> Sub
      DT.Multiply -> Mul
      DT.Divide -> Div
      DT.Modulo -> Div
      DT.Equal -> Eqq
      DT.NotEqual -> Eqq
      DT.LessThan -> Less
      DT.GreaterThan -> Less
      DT.LessThanOrEqual -> Less
      DT.GreaterThanOrEqual -> Less
      DT.And -> Less
      DT.Or -> Less
    unaryOperator op = case op of
      DT.Negate -> Not
      DT.Not -> Not
