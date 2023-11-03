module Main (main) where

import Execute (trancode)
import Parser (parseAst)
import System.Environment (getArgs)
import Writing (writeProgram)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [flag, filename] -> do
      case flag of
        "-c" -> do
          content <- readFile filename
          program <- case parseAst content of
            Just program -> return program
            Nothing -> error $ show $ parseAst content
          putStrLn $ writeProgram $ trancode program
        _ -> error "Invalid flag"
    _ -> error "Usage: ./main <filename>"
