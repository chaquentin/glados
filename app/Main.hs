module Main (main) where

import Execute (printExecute)
import Lib (someFunc)
import Parser (parseAst)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  --   Expect one arg, the string to parse.
  case args of
    [arg] -> do printExecute $ parseAst arg
    _ -> someFunc
