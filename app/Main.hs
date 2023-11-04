module Main (main) where

import Execute (trancode)
import Parser (parseAst)
import Writing (writeProgram)
import System.Environment
import ReadFile
import ParseBin (parseStringToArray, execLeCode)

printHelp :: IO ()
printHelp = putStrLn "Usage ./glados [--VM | --compile] [FILENAME]"
    >> putStrLn "\t --VM: interpret the file"
    >> putStrLn "\t --compile: compile the file to binary"
    >> putStrLn "\t [FILENAME]: file containing instructions"

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["-h"] -> printHelp
        ["--VM", file] -> do
            content <- readFileIfExists file
            case execLeCode $ parseStringToArray content of
                Left err -> putStrLn err
                Right bin ->print bin
        ["--compile", file] -> do
            content <- readFileIfExists file
            program <- case parseAst content of
              Just program -> return program
              Nothing -> error $ show $ parseAst content
            putStrLn $ writeProgram $ trancode program
        _ -> putStrLn "Incorrect use. Use './glados -h' to display help."
