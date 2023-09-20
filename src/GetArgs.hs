{-
-- EPITECH PROJECT, 2023
-- B-FUN-400-TLS-4-1-compressor-thibault.mir
-- File description:
-- GetArgs
-}

module GetArgs
    (   parseArgs,
    ) where

import Exception
import Control.Exception

parseAllArgs :: [String] -> Int
parseAllArgs [] = 0
parseAllArgs ("-h":_) = throw $ SendHelp
parseAllArgs (_) = 1

makeArgument :: Int -> String
makeArgument 0 = ""
makeArgument _ = throw $ BadArgument "Bad usage, try -h"

parseArgs :: [String] -> String
parseArgs [] = ""
parseArgs (s) = makeArgument (parseAllArgs (s))