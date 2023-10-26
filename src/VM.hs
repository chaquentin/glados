{-|
Module      : VM
Description : This module contains the VM of the language.
This module contains the VM of the language and all the functions to execute the program and the functions of the language.
-}
module VM (Value(..), Builtin(..), Instruction(..), Stack, Program, Args, Env, Vars, add, sub, mul, divi, eqq, less, nnot, nnot', jumpIfFalse, getargs, call, changevars, changevarslist, exec) where

-- | This data type represent the value of the language.
data Value
    = Number Int
    | Chaine String
    | Boolean Bool
    | Builtin Builtin
    | Function String Int Program 

-- | This instance is used to show the value of the language.
instance Show Value where
    show (Number n) = show n
    show (Chaine s) = s
    show (Boolean b) = show b
    show (Builtin n) = "<built-in function> " ++ show n
    show (Function n a _) = "<function> " ++ n ++ " <nomber of args> " ++ show a

-- | This instance is used to compare the value of the language.
instance Eq Value where
    (Number n) == (Number n') = n == n'
    (Chaine s) == (Chaine s') = s == s'
    (Boolean b) == (Boolean b') = b == b'
    (Builtin _) == (Builtin _) = True
    (Function n _ _) == (Function n' _ _) = n == n'
    _ == _ = False

-- | This data type represent the built-in function of the language.
data Builtin
    = Add
    | Sub
    | Mul
    | Div
    | Eqq
    | Less
    | Not Builtin
    deriving (Show, Eq)

-- | This data type represent the instruction of the language.
data Instruction
    = Push Value
    | Call
    | JumpIfFalse Int
    | PushArg Int
    | PushEnv String
    | PushVar String
    | ChangeVar String Value
    | Ret

-- | This data type represent the stack of the language.
type Stack = [Value]
-- | This data type represent the program of the language.
type Program = [Instruction]
-- | This data type represent the arguments of the language.
type Args = [Value]
-- | This data type represent the environment of the language.
type Env = [(String, (Int, Program))]
-- | This data type represent the variables of the language.
type Vars = [(String, Value)]

-- | This function take a `Stack` and return a `Stack` or an error.
-- This function add the two first element of the stack and add the result to the stack.
-- If the two first element of the stack is not a 'Number', it return an error.
-- >>> add [Number 2, Number 2]
-- Right [4]
-- >>> add [Number 1, Boolean True]
-- Left "Invalid arguments for add"
-- >>> add [Number 1]
-- Left "Invalid arguments for add"
-- >>> add [Number 1, Number 2, Number 3]
-- Right [3,3]
add :: Stack -> Either String Stack
add (Number x:Number y:xs) = Right $ Number (x + y) : xs
add _ = Left "Invalid arguments for add"

-- | This function take a `Stack` and return a `Stack` or an error.
-- This function sub the second element of the stack to the first element of the stack and add the result to the stack.
-- If the two first element of the stack is not a 'Number', it return an error.
-- >>> sub [Number 1, Number 2]
-- Right [-1]
-- >>> sub [Number 1, Boolean True]
-- Left "Invalid arguments for sub"
-- >>> sub [Number 1]
-- Left "Invalid arguments for sub"
sub :: Stack -> Either String Stack
sub (Number x:Number y:xs) = Right $ Number (x - y) : xs
sub _ = Left "Invalid arguments for sub"

-- | This function take a `Stack` and return a `Stack` or an error.
-- This function mul the two first element of the stack and add the result to the stack.
-- If the two first element of the stack is not a 'Number', it return an error.
-- >>> mul [Number 1, Number 2]
-- Right [2]
-- >>> mul [Number 1, Boolean True]
-- Left "Invalid arguments for mul"
-- >>> mul [Number 1]
-- Left "Invalid arguments for mul"
mul :: Stack -> Either String Stack
mul (Number x:Number y:xs) = Right $ Number (x * y) : xs
mul _ = Left "Invalid arguments for mul"

-- | This function take a `Stack` and return a `Stack` or an error.
-- This function div the second element of the stack by the first element of the stack and add the result to the stack.
-- If the two first element of the stack is not a 'Number', it return an error.
-- If the first element of the stack is 0, it return an error.
-- >>> divi [Number 1, Number 2]
-- Right [2]
-- >>> divi [Number 1, Boolean True]
-- Left "Invalid arguments for div"
-- >>> divi [Number 1]
-- Left "Invalid arguments for div"
-- >>> divi [Number 0, Number 1]
-- Left "Division by zero"
divi :: Stack -> Either String Stack
divi (Number 0:_) = Left "Division by zero"
divi (Number x:Number y:xs) = Right $ Number (y `Prelude.div` x) : xs
divi _ = Left "Invalid arguments for div"

-- | This function take a `Stack` and return a `Stack` or an error.
-- This function check if the two first element of the stack are equal and add the result to the stack.
-- If the two first element of the stack is not the same type, it return an error.
-- >>> eqq [Number 1, Number 2]
-- Right [False]
-- >>> eqq [Number 1, Number 1]
-- Right [True]
-- >>> eqq [Chaine "test", Chaine "test"]
-- Right [True]
-- >>> eqq [Chaine "test", Chaine "test2"]
-- Right [False]
-- >>> eqq [Boolean True, Boolean True]
-- Right [True]
-- >>> eqq [Boolean True, Boolean False]
-- Right [False]
-- >>> eqq [Number 1, Boolean True]
-- Left "Invalid arguments for eq?"
-- >>> eqq [Number 1]
-- Left "Invalid arguments for eq?"
eqq :: Stack -> Either String Stack
eqq (Number x:Number y:xs) = Right $ Boolean (x == y) : xs
eqq (Boolean x:Boolean y:xs) = Right $ Boolean (x == y) : xs
eqq (Chaine x:Chaine y:xs) = Right $ Boolean (x == y) : xs
eqq _ = Left "Invalid arguments for eq?"

-- | This function take a `Stack` and return a `Stack` or an error.
-- This function check if the second element of the stack is less than the first element of the stack and add the result to the stack.
-- If the two first element of the stack is not the same type, it return an error.
-- >>> less [Number 1, Number 2]
-- Right [True]
-- >>> less [Number 1, Number 1]
-- Right [False]
-- >>> less [Number 2, Number 1]
-- Right [False]
-- >>> less [Chaine "test", Chaine "test"]
-- Right [False]
-- >>> less [Chaine "test", Chaine "test2"]
-- Right [True]
-- >>> less [Boolean True, Boolean True]
-- Left "Invalid arguments for less"
-- >>> less [Number 1, Boolean True]
-- Left "Invalid arguments for less"
less :: Stack -> Either String Stack
less (Number x:Number y:xs) = Right $ Boolean (y < x) : xs
less (Chaine x:Chaine y:xs) = Right $ Boolean (y < x) : xs
less _ = Left "Invalid arguments for less"

-- | This function take a `Bool` and return a `Bool`.
-- This function return the opposite of the `Bool`.
-- >>> nnot True
-- False
-- >>> nnot False
-- True
nnot' :: Bool -> Bool
nnot' True = False
nnot' False = True

-- | This function take a `Builtin`, `Stack` and return a `Stack` or an error.
-- This function check if the 'Builtin' is 'Eqq' or 'Less' and call the function 'eqq' or 'less' with the stack and return the opposite of the result.
-- If the 'Builtin' is not 'Eqq' or 'Less', it return an error.
-- >>> nnot Eqq [Number 1, Number 2]
-- Right [True]
-- >>> nnot Eqq [Number 1, Number 1]
-- Right [False]
-- >>> nnot Eqq [Number 2, Number 1]
-- Right [True]
-- >>> nnot Less [Number 1, Number 2]
-- Right [True]
-- >>> nnot Less [Number 1, Number 1]
-- Right [True]
-- >>> nnot Less [Number 2, Number 1]
-- Right [False]
-- >>> nnot Add [Chaine "test", Chaine "test"]
-- Left "Invalid arguments for not"
nnot :: Builtin -> Stack -> Either String Stack
nnot b s
    | b == Eqq = case eqq s of
        Left err -> Left err
        Right ((Boolean bo):xs) -> Right $ Boolean (nnot' bo) : xs
        _ -> Left "Invalid arguments for not"
    | b == Less = case less s of
        Left err -> Left err
        Right ((Boolean bo):xs) -> Right $ Boolean (nnot' bo) : xs
        _ -> Left "Invalid arguments for not"
    | otherwise = Left "Invalid arguments for not"

-- | This function take a `Int`, `Stack` and 'Program' and return a `Program` or an error.
-- This function jump to the instruction at the index `Int` if the first element of the stack is `False`.
-- If the first element of the stack is not a 'Boolean', it return an error.
-- >>> jumpIfFalse 1 [Boolean False] [Push (Number 1), Push (Number 2)]
-- Right [Push (Number 2)]
-- >>> jumpIfFalse 1 [Boolean True] [Push (Number 1), Push (Number 2)]
-- Right [Push (Number 1),Push (Number 2)]
-- >>> jumpIfFalse 1 [Number 1] [Push (Number 1), Push (Number 2)]
-- Left "Invalid arguments for jumpIfFalse"
jumpIfFalse :: Int -> Stack -> Program -> Either String Program
jumpIfFalse n (Boolean False:_) p = Right (drop n p)
jumpIfFalse _ (Boolean True:_) p = Right p
jumpIfFalse _ _ _= Left "Invalid arguments for jumpIfFalse"

-- | This function take a `Int` and a `Stack` and return a `Args`.
-- This function return the `Int` first element of the stack.
-- >>> getargs 1 [Number 1, Number 2]
-- [Number 1]
-- >>> getargs 2 [Number 1, Number 2]
-- [Number 1, Number 2]
-- >>> getargs 3 [Number 1, Number 2]
-- [Number 1, Number 2]
getargs :: Int -> Stack -> Args
getargs 0 _ = []
getargs _ [] = []
getargs n (x:xs) = x : (x:getargs (n - 1) xs)

-- | This function take a `Vars`, `Env`, `Value` and a `Stack` and return a `Stack` or an error.
-- This function call the first element of the stack and return the result.
-- If the first element of the stack is not a 'Function' or a 'Builtin', it return an error.
-- >>> call [] [] (Builtin Add) [Number 1, Number 2]
-- Right [3]
-- >>> call [] [] (Number 1) [Number 1, Number 2]
-- Left "Invalid call"
call :: Vars -> Env -> Value -> Stack -> Either String Stack
call _ _ (Builtin Add) s = add s
call _ _ (Builtin Sub) s = sub s
call _ _ (Builtin Mul) s = mul s
call _ _ (Builtin Div) s = divi s
call _ _ (Builtin Eqq) s = eqq s
call _ _ (Builtin Less) s = less s
call _ _ (Builtin (Not b)) s = nnot b s
call vars env (Function _ a p) (s:xs) = case exec (getargs a (s:xs)) env vars p [] of
    Left err -> Left err
    Right (Number n) -> Right $ Number n : xs
    Right (Boolean b) -> Right $ Boolean b : xs
    Right (Function n ar pro) -> Right $ Function n ar pro : xs
    Right (Builtin b) -> Right $ Builtin b : xs
    Right (Chaine str) -> Right $ Chaine str : xs
call _ _ _ _ = Left "Invalid call"

-- | This function take a `Vars`, `String` and a `Value` and return a `Vars`.
-- This function change the value of the variable `String` to the `Value`.
-- >>> changevars [("test", Number 1)] "test" (Number 2)
-- [("test",Number 2)]
-- >>> changevars [("test", Number 1)] "test2" (Number 2)
-- [("test",Number 1)]
changevars :: Vars -> String -> Value -> Vars
changevars [] _ _ = []
changevars ((n, v): xs) name nvalue
    | n == name = (name, nvalue) : xs
    | otherwise = (n, v) : changevars xs name nvalue

-- | This function take a `Vars`, `String` and a `Value` and return a `Vars`.
-- This function change the value of the variable `String` to the `Value` if the variable exist.
-- If the variable doesn't exist, it add the variable to the `Vars`.
-- >>> changevarslist [("test", Number 1)] "test" (Number 2)
-- [("test",Number 2)]
-- >>> changevarslist [("test", Number 1)] "test2" (Number 2)
-- [("test2",Number 2),("test",Number 1)]
changevarslist :: Vars -> String -> Value -> Vars
changevarslist vars name nvalue = case lookup name vars of
    Just _ -> changevars vars name nvalue
    Nothing -> (name, nvalue) : vars

-- | This function take an `Args` and an `Env` and return a `Vars`
-- This function execute all the 'Instruction' of the 'Program' and return the result.
-- If the 'Program' is not finish by the 'Instruction' 'Ret', it return an error.
-- If for the 'Instruction' 'PushEnv' the 'String' is not in the 'Env', it return an error.
-- If for the 'Instruction' 'PushVar' the 'String' is not in the 'Vars', it return an error.
-- >>> exec [] [] [] [Push (Number 1), Push (Number 2), Push (Builtin Add), Call, Ret] []
-- Right 3
-- >>> exec [] [] [] [Push (Number 1), Push (Number 2), Push (Builtin Sub), Call] []
-- Left "Invalid programe"
-- >>> exec [] [] [] [PushVar "test", Push (Number 2), Push (Builtin Add), Call, Ret] []
-- Left "Invalid push var"
-- >>> exec [] [] [] [PushEnv "test", Push (Number 2), Push (Builtin Add), Call, Ret] []
-- Left "Invalid env"
exec :: Args -> Env -> Vars -> Program -> Stack -> Either String Value
exec a env vars ((Push v):xs) s = exec a env vars xs (v : s)
exec a env vars ((PushArg n):xs) s = exec a env vars xs ((a !! n) : s)
exec a env vars ((PushEnv n):xs) s = case lookup n env of
    Nothing -> Left "Invalid env"
    Just (ar, p) -> exec a env vars xs (Function n ar p : s)
exec a env vars ((PushVar n):xs) s = case lookup n vars of
    Nothing -> Left "Invalid push var"
    Just v -> exec a env vars xs (v : s)
exec a env vars ((ChangeVar n v):xs) s = exec a env (changevarslist vars n v) xs s
exec a env vars (Call:xs) s = case call vars env (head s) (tail s) of
    Left err -> Left err
    Right p -> exec a env vars xs p
exec a env vars ((JumpIfFalse n):xs) s = case jumpIfFalse n s xs of
    Left err -> Left err
    Right p -> exec a env vars p s
exec _ _ _ (Ret:_) (v:_) = Right v
exec _ _ _ _ _ = Left "Invalid programe"
