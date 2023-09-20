{-
-- EPITECH PROJECT, 2023
-- B-FUN-400-TLS-4-1-compressor-thibault.mir
-- File description:
-- Exception
-}

module Exception
    (   ICExceptions (..),
        exceptionHandler
    ) where

import Control.Exception (Exception)
import System.Exit

data ICExceptions = SendHelp
                | BadArgument String
                | FileReaderException String
                | Run String
                | Syntax String
                deriving (Show)

instance Exception ICExceptions

sendHelp :: IO ()
sendHelp = mapM_ putStrLn ["USAGE: ./glados < file.scm\n",
                "\tfile.scm\tfile containing instructions to interpret"]

exceptionHandler :: ICExceptions -> IO ()
exceptionHandler SendHelp = sendHelp >> exitSuccess
exceptionHandler (BadArgument s) = putStrLn ("Bad argument : " ++ s) >>
                                            exitWith (ExitFailure 84)
exceptionHandler (Syntax s) = putStrLn ("*** ERROR : " ++ s) >>
                                            exitWith (ExitFailure 84)
exceptionHandler (FileReaderException s) = putStrLn ("Invalid file : " ++ s) >>
                                            exitWith (ExitFailure 84)
exceptionHandler (Run s) = putStrLn ("Run error : " ++ s) >>
                                            exitWith (ExitFailure 84)
