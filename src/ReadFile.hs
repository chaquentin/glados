{-
-- EPITECH PROJECT, 2023
-- B-FUN-500-TLS-5-1-glados-quentin.challon
-- File description:
-- readfile
-}

module ReadFile (readFileIfExists) where
import System.IO

readFileIfExists :: String -> IO ()
readFileIfExists filename = do
    fileContents <- readFileSafe filename
    case fileContents of
        Just contents -> do
            putStrLn "file content :"
            putStrLn contents
        Nothing -> putStrLn $ "file " ++ filename ++ " does not exist."

readFileSafe :: String -> IO (Maybe String)
readFileSafe filename = do
    handle <- openFile filename ReadMode
    contents <- hGetContents handle
    return (Just contents)
