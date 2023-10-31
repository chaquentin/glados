{-
-- EPITECH PROJECT, 2023
-- B-FUN-500-TLS-5-1-glados-quentin.challon
-- File description:
-- readfile
-}

module ReadFile (readFileIfExists, fileToBin) where
import System.IO
import System.Directory
import System.Exit (exitWith, ExitCode(..))

readFileIfExists :: String -> IO String
readFileIfExists filename = do
    fileExists <- doesFileExist filename
    if fileExists
        then do
            filePermissions <- getPermissions filename
            if readable filePermissions
                then do
                    fileContents <- readFileSafe filename
                    case fileContents of
                        Just contents -> 
                            if contents /= ""
                                then return contents
                                else do
                                    putStrLn $ "File " ++ filename ++ " is empty."
                                    exitWith (ExitFailure 84)
                        Nothing -> do
                            putStrLn $ "File " ++ filename ++ " is empty."
                            exitWith (ExitFailure 84)
                else do
                    putStrLn $ "You don't have read permissions for file " ++ filename ++ "."
                    exitWith (ExitFailure 84)
        else do
            putStrLn $ "File " ++ filename ++ " does not exist."
            exitWith (ExitFailure 84)

readFileSafe :: String -> IO (Maybe String)
readFileSafe filename = do
    handle <- openFile filename ReadMode
    contents <- hGetContents handle
    return (Just contents)
