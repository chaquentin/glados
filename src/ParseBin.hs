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

arrayToProgram :: [String] -> Program -> Program
arrayToProgram [] _ = []
arrayToProgram (x:xs) p | instruction == "push" = arrayToProgram xs (p ++ [Push (Number (read (drop 5 x) :: Int))])
                        | instruction == "call" = arrayToProgram xs (p ++ [Call])
                        | otherwise = arrayToProgram xs p
                        where (instruction, rest) = cutFirstWord $ removeIndemptation x
