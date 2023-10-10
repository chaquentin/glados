{-# LANGUAGE LambdaCase #-}

module Execute
  ( printExecute,
    Env,
  )
where

import Data.Fixed (mod')
import qualified Data.Map as Map
import DataTypes (Ast (..))

type Env = Map.Map String Ast

-- | Execute an AST in an environment.
execute :: Ast -> Env -> IO Ast
-- If the expression is a number, boolean, or null, return it.
execute (Number n) _ = return $ Number n
execute (Boolean b) _ = return $ Boolean b
execute Null _ = return Null
-- If the expression is a variable, look it up in the environment.
execute (Variable v) env = maybe (error $ "Variable " ++ v ++ " not found.") return (Map.lookup v env)
-- execute (List [Define name expr]) env = Map.insert name expr env `seq` return Null
execute (List [Define name expr]) env = case length . words $ name of
  -- if name is one word, then it's a variable definition, so we add it to the environment, nothing else
  1 -> Map.insert name expr env `seq` return Null
  -- if name is more than one word, then it's a function definition, so we build a Function and add it to the environment, nothing else
  _ -> Map.insert name (Function (words name) expr) env `seq` return Null
execute (List [If c t e]) env = do
  Boolean condition <- execute c env
  if condition then execute t env else execute e env
execute (List [Lambda args body]) _ = return $ Lambda args body
execute (List (f : args)) env = do
  function <- execute f env
  case function of
    BuiltIn f' -> do
      argVals <- mapM (`execute` env) args
      return $ f' argVals
    Lambda argNames body -> do
      argVals <- mapM (`execute` env) args
      let newEnv = Map.fromList $ zip argNames argVals
      execute body (Map.union newEnv env)
    Function argNames body -> do
      argVals <- mapM (`execute` env) args
      let newEnv = Map.fromList $ zip argNames argVals
      execute body (Map.union newEnv env)
    _ -> error "First element of list must be a function."
execute _ _ = error "Invalid expression."

-- | Execute an AST in an environment with built-in functions.
buildInExecute :: Ast -> IO Ast
buildInExecute ast = execute ast buildInFunctions

-- Now as a map
buildInFunctions :: Env
buildInFunctions =
  Map.fromList
    [ ("+", BuiltIn plus),
      ("-", BuiltIn minus),
      ("*", BuiltIn times),
      ("div", BuiltIn divi),
      ("mod", BuiltIn modd),
      ("eq?", BuiltIn eq),
      ("<", BuiltIn inf)
    ]

-- | A function that checks if two values are equal.
eq :: [Ast] -> Ast
eq [Number n1, Number n2] = Boolean $ n1 == n2
eq [Boolean b1, Boolean b2] = Boolean $ b1 == b2
eq [Null, Null] = Boolean True
eq _ = Boolean False

-- | A function that adds n numbers.
plus :: [Ast] -> Ast
plus = Number . sum . map (\case Number n -> n; _ -> error "Argument must be a number.")

-- | A function that subtracts 2 numbers, or negates a number.
minus :: [Ast] -> Ast
minus [Number n] = Number $ -n
minus [Number n1, Number n2] = Number $ n1 - n2
minus _ = error "Arguments must be numbers."

-- | A function that multiplies n numbers.
times :: [Ast] -> Ast
times = Number . product . map (\case Number n -> n; _ -> error "Argument must be a number.")

-- | A function that divides 2 numbers.
divi :: [Ast] -> Ast
divi [Number n1, Number n2] = Number $ n1 / n2
divi _ = error "Arguments must be numbers."

-- | A function that calculates the remainder of 2 numbers.
modd :: [Ast] -> Ast
modd [Number n1, Number n2] = Number $ n1 `mod'` n2
modd _ = error "Arguments must be numbers."

-- | A function that checks if the first number is smaller than the second.
inf :: [Ast] -> Ast
inf [Number n1, Number n2] = Boolean $ n1 < n2
inf _ = error "Arguments must be numbers."

-- | Execute an AST in an empty environment and print the result.
printExecute :: Ast -> IO ()
printExecute ast = print =<< buildInExecute ast