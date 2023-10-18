module VM where

data Value
    = Number Int
    | Boolean Bool
    | Builtin Builtin
    | Function String Int Program 

instance Show Value where
    show (Number n) = show n
    show (Boolean b) = show b
    show (Builtin n) = "<built-in function> " ++ show n
    show (Function n a p) = "<function> " ++ n ++ " <nomber of args> " ++ show a

instance Eq Value where
    (Number n) == (Number n') = n == n'
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
    deriving (Show, Eq)

data Instruction
    = Push Value
    | Call
    | JumpIfFalse Int
    | PushArg Int
    | PushEnv String
    | Ret

type Stack = [Value]
type Program = [Instruction]
type Args = [Value]
type Env = [(String, (Int, Program))]

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

less :: Stack -> Either String Stack
less (Number x:Number y:xs) = Right $ Boolean (y < x) : xs
less _ = Left "Invalid arguments for less"

jumpIfFalse :: Int -> Stack -> Program -> Either String Program
jumpIfFalse n (Boolean False:_) p = Right (drop n p)
jumpIfFalse _ (Boolean True:_) p = Right p
jumpIfFalse _ _ _= Left "Invalid arguments for jumpIfFalse"

getargs :: Int -> Stack -> Args
getargs 0 _ = []
getargs n (x:xs) = x : (x:getargs (n - 1) xs)

call :: Env -> Value -> Stack -> Either String Stack
call _ (Builtin Add) s = add s
call _ (Builtin Sub) s = sub s
call _ (Builtin Mul) s = mul s
call _ (Builtin Div) s = divi s
call _ (Builtin Eqq) s = eqq s
call _ (Builtin Less) s = less s
call env (Function _ a p) (s:xs) = case exec (getargs a (s:xs)) env p [] of
    Left err -> Left err
    Right (Number n) -> Right $ Number n : xs
    Right (Boolean b) -> Right $ Boolean b : xs
    Right (Function n ar p) -> Right $ Function n ar p : xs
    Right (Builtin b) -> Right $ Builtin b : xs
call _ _ _ = Left "Invalid call"

eqq :: Stack -> Either String Stack
eqq (Number x:Number y:xs) = Right $ Boolean (x == y) : xs
eqq (Boolean x:Boolean y:xs) = Right $ Boolean (x == y) : xs
eqq _ = Left "Invalid arguments for eq?"

exec :: Args -> Env -> Program -> Stack -> Either String Value
exec a env ((Push v):xs) s = exec a env xs (v : s)
exec a env ((PushArg n):xs) s = exec a env xs ((a !! n) : s)
exec a env (Call:xs) s = case call env (head s) (tail s) of
    Left err -> Left err
    Right p -> exec a env xs p
exec a env ((JumpIfFalse n):xs) s = case jumpIfFalse n s xs of
    Left err -> Left err
    Right p -> exec a env p s
exec a env ((PushEnv n):xs) s = case lookup n env of
    Nothing -> Left "Invalid env"
    Just (ar, p) -> exec a env xs (Function n ar p : s)
exec a env (Ret:_) (v:_) = Right v
exec _ _ _ _ = error "Invalid progra"
