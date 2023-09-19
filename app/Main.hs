{-
-- EPITECH PROJECT, 2023
-- glados
-- File description:
-- Main
-}

module Main (main) where

import Lib
import DataTypes

main :: IO ()
main = do
    contents <- getContents
    putStrLn "Input:"
    putStrLn contents
