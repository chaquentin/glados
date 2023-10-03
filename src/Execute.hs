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
execute (Variable v) env = case Map.lookup v env of
  Just x -> return x
  Nothing -> error $ "Variable " ++ v ++ " not found."
-- If the expression is Lambda, return it.
execute (List [Define _ e]) env = do
  execute e env
execute (List [If c t e]) env = do
  x <- execute c env
  case x of
    Boolean True -> execute t env
    Boolean False -> execute e env
    _ -> error "Condition must be a boolean."
execute (List [Lambda args body]) _ = return $ Lambda args body
execute (List (f : args)) env = do
  x <- execute f env
  case x of
    Lambda argNames body -> do
      argVals <- mapM (`execute` env) args
      let newEnv = Map.fromList $ zip argNames argVals
      execute body (Map.union newEnv env)
    _ -> error "First element of list must be a function."
execute _ _ = error "Invalid expression."

-- | Execute an AST in an empty environment.
emptyExecute :: Ast -> IO Ast
emptyExecute ast = execute ast Map.empty

-- | Execute an AST in an empty environment and print the result.
printExecute :: Ast -> IO ()
printExecute ast = print =<< emptyExecute ast