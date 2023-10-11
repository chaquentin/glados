{-
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- Main
-}

module Main (main) where
import System.Environment
import ReadFile

printHelp :: IO ()
printHelp = putStrLn "Usage ./glados [FILENAME]"
    >> putStrLn "\t [FILENAME]: file containing instructions to interpret"

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["-h"] -> printHelp
        [filename] -> readFileIfExists filename
        _ -> putStrLn "Incorrect use. Use './glados -h' to display help."

