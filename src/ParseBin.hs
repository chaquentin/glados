import VM
myBreak :: String -> (String, String)
myBreak [] = ([], [])
myBreak (x:xs) | x == '\n' = ([], xs)
               | otherwise = (x: before, after)
               where (before, after) = myBreak xs

parseStringToArray :: String -> [String]
parseStringToArray input = case myBreak input of
    (before, after) | null after -> [before]
                    | otherwise -> (before : parseStringToArray after)

startWith :: String -> String -> Bool
startWith [] _ = True
startWith _ [] = False
startWith (x:xs) (y:ys) | x == y = startWith xs ys
                        | otherwise = False

findMain :: [String] -> Either String [String]
findMain [] = Left "Error: need a main function"
findMain (x:xs) = case startWith x "main:" of
    True -> Right xs
    _ -> findMain xs

removeIndemptation :: String -> String
removeIndemptation [] = []
removeIndemptation (x:xs) | x == ' ' = removeIndemptation xs
                          | x == '\t' = removeIndemptation xs
                          | otherwise = x:xs

cutFirstWord :: String -> (String, String)
cutFirstWord [] = ([], [])
cutFirstWord (x:xs) | x == ' ' = ([], removeIndemptation xs)
                    | x == '\t' = ([], removeIndemptation xs)
                    | otherwise = (x: before, after)
                    where (before, after) = cutFirstWord xs

-- stringToInstruction :: String -> Instruction
-- stringToInstruction str 

getEndOfString :: String -> Either String (String, String)
getEndOfString [] = Left "Error: no end of string"
getEndOfString (x:xs) | x == '"' = Right ([], xs)
                      | otherwise = case getEndOfString xs of
                        Right (str, rest) -> Right ((x:str), rest)
                        Left err -> Left err

getString :: String -> Either String (String, String)
getString [] = Left "Error: no start of string"
getString (x:xs) | x == '"' = getEndOfString xs
                 | otherwise = Left "Error: no start of string"

getNumber :: String -> (String, String)
getNumber [] = ([], [])
getNumber (x:xs) | x >= '0' && x <= '9' = (x: before, after)
                 | otherwise = ([], x:xs)
                 where (before, after) = getNumber xs

getBool :: String -> Either String Bool
getBool str = case cutFirstWord str of
    ("true", _) -> Right True
    ("false", _) -> Right False
    _ -> Left "Error: not a boolean"

restToValue :: String -> Value
restToValue str = case getString str of
    Right (str, _) -> Chaine str
    Left err -> case getNumber str of
        ([], _) -> case getBool str of
            Right b -> Boolean b
            Left err -> error err
        (str, _) -> Number (read str :: Int)

arrayToProgram :: [String] -> Program -> Program
arrayToProgram [] _ = []
arrayToProgram (x:xs) p | instruction == "push" = arrayToProgram xs (p ++ [Push (Number (read rest :: Int))])
                        | instruction == "call" = arrayToProgram xs (p ++ [Call])
                        | otherwise = arrayToProgram xs p
                        where (instruction, rest) = cutFirstWord $ removeIndemptation x
