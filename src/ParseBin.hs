module ParseBin where

import VM
import Data.Char
import Control.Arrow (Arrow(first))

myBreak :: String -> (String, String)
myBreak [] = ([], [])
myBreak (x:xs) | x == '\n' = ([], xs)
               | otherwise = (x: before, after)
               where (before, after) = myBreak xs

parseStringToArray :: String -> [String]
parseStringToArray input = case myBreak input of
    (before, after) | null after -> [before]
                    | otherwise -> before : parseStringToArray after

startWith :: String -> String -> Bool
startWith [] _ = True
startWith _ [] = False
startWith (x:xs) (y:ys) | x == y = startWith xs ys
                        | otherwise = False

findMain :: [String] -> Either String [String]
findMain [] = Left "Error: need a main function"
findMain (x:xs) | startWith x "main:" = Right xs
                | otherwise = findMain xs

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
                        Right (str, rest) -> Right (x:str, rest)
                        Left err -> Left err

getString :: String -> Either String (String, String)
getString [] = Left "Error: no start of string"
getString (x:xs) | x == '"' = getEndOfString xs
                 | otherwise = Left "Error: no start of string"

getNumber :: String -> (String, String)
getNumber [] = ([], [])
getNumber (x:xs) | isDigit x = (x: before, after)
                 | otherwise = ([], x:xs)
                 where (before, after) = getNumber xs

getBool :: String -> Either String Bool
getBool str = case cutFirstWord $ removeIndemptation str of
    ("true", _) -> Right True
    ("false", _) -> Right False
    _ -> Left "Error: not a boolean"

getBuiltin :: String -> Either String Builtin
getBuiltin str = case cutFirstWord $ removeIndemptation str of
    ("add", _) -> Right Add
    ("sub", _) -> Right Sub
    ("mul", _) -> Right Mul
    ("div", _) -> Right Div
    ("eq", _) -> Right Eqq
    ("less", _) -> Right Less
    ("not", xs) -> case getBuiltin xs of
        Right b -> Right (Not b)
        Left err -> Left err
    _ -> Left "Error: not a builtin"

restToValue :: String -> Either String Value
restToValue str = case getString $ removeIndemptation str of
    Right (str', _) -> Right (Chaine str')
    Left _ -> case getNumber str of
        ([], _) -> case getBool str of
            Right b -> Right (Boolean b)
            Left _ -> case getBuiltin str of
                Right b -> Right (Builtin b)
                Left err -> Left err
        (str', _) -> Right (Number (read str' :: Int))

arrayToProgram :: [String] -> Program -> Program
arrayToProgram [] _ = error "Error: no end of program"
arrayToProgram (x:xs) p | instruction == "push" = case restToValue rest of
                            Right v -> arrayToProgram xs (p ++ [Push v])
                            Left err -> error err
                        | instruction == "call" = arrayToProgram xs (p ++ [Call])
                        | instruction == "ret" = arrayToProgram xs (p ++ [Ret])
                        | instruction == "end" = p
                        | instruction == "jmpiffalse" = case restToValue rest of
                            Right (Number v) -> arrayToProgram xs (p ++ [JumpIfFalse v])
                            _ -> error "Error: not a number"
                        | instruction == "pusharg" = case restToValue rest of
                            Right (Number v) -> arrayToProgram xs (p ++ [PushArg v])
                            _ -> error "Error: not a number"
                        | instruction == "pushenv" = arrayToProgram xs (p ++ [PushEnv $ fst (cutFirstWord (removeIndemptation rest))])
                        | instruction == "pushvar" = arrayToProgram xs (p ++ [PushVar $ fst (cutFirstWord (removeIndemptation rest))])
                        | instruction == "changevar" = case cutFirstWord $ removeIndemptation rest of
                            (name, rest') -> case restToValue rest' of
                                Right v -> arrayToProgram xs (p ++ [ChangeVar name v])
                                Left err -> error err
                        | otherwise = arrayToProgram xs p
                        where (instruction, rest) = cutFirstWord $ removeIndemptation x

getFunction :: String -> Either String (String, Int)
getFunction line = case cutFirstWord $ removeIndemptation line of
    (name, rest) -> case getNumber $ removeIndemptation rest of
        (nb, rest') -> case cutFirstWord $ removeIndemptation rest' of
            (":", _) -> Right (name, read nb :: Int)
            _ -> Left "Error: not a function"

generateEnv :: [String] -> Env -> Env
generateEnv [] env = env
generateEnv (x:xs) env = case getFunction x of
    Right (name, nb) -> generateEnv xs (env ++ [(name, (nb, arrayToProgram xs []))])
    Left _ -> generateEnv xs env


execLeCode :: [String] -> Either String Value
execLeCode code = case findMain code of
    Right main -> exec [] (generateEnv code []) [] (arrayToProgram main []) []
    Left err -> Left err
