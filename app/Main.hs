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
            putStrLn content
            putStrLn "a mettre ici la fonction de parse du binaire"
        ["--compile", file] -> do
            content <- readFileIfExists file
            putStrLn content
            putStrLn "a mettre ici la fonction de parse du fichier pour le compilo"
        _ -> putStrLn "Incorrect use. Use './glados -h' to display help."
