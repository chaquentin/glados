module VM where

data Value
    = Number Int
    | Chaine String
    | Boolean Bool
    | Builtin Builtin
    | Function String Int Program 

instance Show Value where
    show (Number n) = show n
    show (Chaine s) = s
    show (Boolean b) = show b
    show (Builtin n) = "<built-in function> " ++ show n
    show (Function n a p) = "<function> " ++ n ++ " <nomber of args> " ++ show a

instance Eq Value where
    (Number n) == (Number n') = n == n'
    (Chaine s) == (Chaine s') = s == s'
    (Boolean b) == (Boolean b') = b == b'
    (Builtin _) == (Builtin _) = True
    (Function n _ _) == (Function n' _ _) = n == n'
    _ == _ = False

data Builtin
    = Add
    | Sub
    | Mul
    | Div
    | Eqq
    | Less
    | Not Builtin
    deriving (Show, Eq)

data Instruction
    = Push Value
    | Call
    | JumpIfFalse Int
    | PushArg Int
    | PushEnv String
    | PushVar String
    | ChangeVar String Value
    | Ret

type Stack = [Value]
type Program = [Instruction]
type Args = [Value]
type Env = [(String, (Int, Program))]
type Vars = [(String, Value)]

add :: Stack -> Either String Stack
add (Number x:Number y:xs) = Right $ Number (x + y) : xs
add _ = Left "Invalid arguments for add"

sub :: Stack -> Either String Stack
sub (Number x:Number y:xs) = Right $ Number (x - y) : xs
sub _ = Left "Invalid arguments for sub"

mul :: Stack -> Either String Stack
mul (Number x:Number y:xs) = Right $ Number (x * y) : xs
mul _ = Left "Invalid arguments for mul"

divi :: Stack -> Either String Stack
divi (Number 0:_) = Left "Division by zero"
divi (Number x:Number y:xs) = Right $ Number (y `Prelude.div` x) : xs
divi _ = Left "Invalid arguments for div"

eqq :: Stack -> Either String Stack
eqq (Number x:Number y:xs) = Right $ Boolean (x == y) : xs
eqq (Boolean x:Boolean y:xs) = Right $ Boolean (x == y) : xs
eqq (Chaine x:Chaine y:xs) = Right $ Boolean (x == y) : xs
eqq _ = Left "Invalid arguments for eq?"

less :: Stack -> Either String Stack
less (Number x:Number y:xs) = Right $ Boolean (y < x) : xs
less (Chaine x:Chaine y:xs) = Right $ Boolean (y < x) : xs
less _ = Left "Invalid arguments for less"

nnot :: Bool -> Bool
nnot True = False
nnot False = True

jumpIfFalse :: Int -> Stack -> Program -> Either String Program
jumpIfFalse n (Boolean False:_) p = Right (drop n p)
jumpIfFalse _ (Boolean True:_) p = Right p
jumpIfFalse _ _ _= Left "Invalid arguments for jumpIfFalse"

getargs :: Int -> Stack -> Args
getargs 0 _ = []
getargs n (x:xs) = x : (x:getargs (n - 1) xs)

call :: Vars -> Env -> Value -> Stack -> Either String Stack
call _ _ (Builtin Add) s = add s
call _ _ (Builtin Sub) s = sub s
call _ _ (Builtin Mul) s = mul s
call _ _ (Builtin Div) s = divi s
call _ _ (Builtin Eqq) s = eqq s
call _ _ (Builtin Less) s = less s
call vars env (Builtin (Not b)) s
    | b == Eqq = case eqq s of
        Left err -> Left err
        Right ((Boolean b):xs) -> Right $ Boolean (nnot b) : s
        _ -> Left "Invalid arguments for not"
    | b == Less = case less s of
        Left err -> Left err
        Right ((Boolean b):xs) -> Right $ Boolean (nnot b) : s
        _ -> Left "Invalid arguments for not"
    | otherwise = Left "Invalid arguments for not"
call vars env (Function _ a p) (s:xs) = case exec (getargs a (s:xs)) env vars p [] of
    Left err -> Left err
    Right (Number n) -> Right $ Number n : xs
    Right (Boolean b) -> Right $ Boolean b : xs
    Right (Function n ar p) -> Right $ Function n ar p : xs
    Right (Builtin b) -> Right $ Builtin b : xs
call _ _ _ _ = Left "Invalid call"

changevars :: Vars -> String -> Value -> Vars
changevars [] _ _ = []
changevars ((n, v): xs) name nvalue
    | n == name = (name, nvalue) : xs
    | otherwise = (n, v) : changevars xs name nvalue

changevarslist :: Vars -> String -> Value -> Vars
changevarslist vars name nvalue = case lookup name vars of
    Just v -> changevars vars name nvalue
    Nothing -> ((name, nvalue):vars)

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
exec a env vars (Ret:_) (v:_) = Right v
exec _ _ _ _ _ = Left "Invalid programe"
