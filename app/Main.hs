{-
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- Main
-}

module Main (main) where

import DataTypes
import GetArgs
import Exception
import System.Environment
import Control.Exception

main :: IO ()
main = handle exceptionHandler $ do
    argv <- getArgs
    contents <- getContents
    let args = parseArgs argv
    if (args /= "")
        then print args
        else print contents
