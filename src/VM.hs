module VM where

data Value
    = Number Int
    | Boolean Bool
    deriving (Show, Eq)

data Builtin
    = Add
    | Sub
    | Mul
    | Div
    | Eqq
    | Less

data Instruction
    = Push Value
    | Call Builtin
    | JumpIfFalse Int
    | Ret

type Stack = [Value]
type Program = [Instruction]

add :: Stack -> Either String Stack
add (Number x:Number y:xs) = Right $ Number (x + y) : xs
add _ = Left "Invalid arguments for add"

sub :: Stack -> Either String Stack
sub (Number x:Number y:xs) = Right $ Number (y - x) : xs
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

call :: Builtin -> Stack -> Either String Stack
call Add = add
call Sub = sub
call Mul = mul
call Div = divi
call Eqq = eqq
call Less = less

eqq :: Stack -> Either String Stack
eqq (Number x:Number y:xs) = Right $ Boolean (x == y) : xs
eqq (Boolean x:Boolean y:xs) = Right $ Boolean (x == y) : xs
eqq _ = Left "Invalid arguments for eq?"

exec :: Program -> Stack -> Either String Value
exec ((Push v):xs) s = exec xs (v : s)
exec ((Call x):xs) s = case call x s of
    Left err -> Left err
    Right s' -> exec xs s'
exec ((JumpIfFalse n):xs) s = case jumpIfFalse n s xs of
    Left err -> Left err
    Right p -> exec p s
exec (Ret:_) (v:_) = Right v
exec _ _ = error "Invalid progra"