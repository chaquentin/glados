{-# LANGUAGE LambdaCase #-}

module Execute
  ( printExecute,
    Env,
  )
where

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
execute (List [Define name expr]) env = Map.insert name <$> execute expr env <*> pure env >>= execute expr
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
      ("inc", BuiltIn inc),
      ("eq?", BuiltIn eq)
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

inc :: [Ast] -> Ast
inc [Number n] = Number $ n + 1
inc _ = error "Argument must be a number."

-- | Execute an AST in an empty environment and print the result.
printExecute :: Ast -> IO ()
printExecute ast = print =<< buildInExecute ast