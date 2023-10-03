{-
-- EPITECH PROJECT, 2023
-- B-FUN-500-TLS-5-1-glados-quentin.challon
-- File description:
-- readfile
-}

module ReadFile (readFileIfExists) where
import System.IO
import System.Directory

readFileIfExists :: String -> IO ()
readFileIfExists filename = do
    fileExists <- doesFileExist filename
    if fileExists
        then do
            filePermissions <- getPermissions filename
            if readable filePermissions
                then do
                    fileContents <- readFileSafe filename
                    case fileContents of
                        Just contents -> do
                            putStrLn "File contents :"
                            putStrLn contents
                        Nothing -> putStrLn $ "File " ++ filename ++ " is empty."
                else putStrLn $ "You don't have read permissions for file " ++ filename ++ "."
        else putStrLn $ "File " ++ filename ++ " does not exist."

readFileSafe :: String -> IO (Maybe String)
readFileSafe filename = do
    handle <- openFile filename ReadMode
    contents <- hGetContents handle
    return (Just contents)
