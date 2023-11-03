module Writing
  ( writeProgram,
  )
where

import Execute (Instruction (..), Program, Value (..))

-- factorial 1:
--     pusharg 0
--     push 1
--     push eq
--     call
--     jmpiffalse 2
--     push 1
--     ret
--     push 1
--     pusharg 0
--     push sub
--     call
--     pushenv factorial
--     call
--     pusharg 0
--     push mul
--     call
--     ret
--     end

-- main:
--     push 5
--     pushenv factorial
--     call
--     ret
--     end

writeInstruction :: Instruction -> String
writeInstruction (Push (Number n)) = "push " ++ show n
writeInstruction (Push (Chaine s)) = "push " ++ show s
writeInstruction (Push (Boolean b)) = "push " ++ show b
writeInstruction (Push (Builtin b)) = "push " ++ show b
writeInstruction (Push (Function name args program)) = "push " ++ name ++ " " ++ show args ++ " " ++ writeProgram program ++ "end"
writeInstruction Call = "call"
writeInstruction (JumpIfFalse n) = "jmpiffalse " ++ show n
writeInstruction (PushArg n) = "pusharg " ++ show n
writeInstruction (PushEnv name) = "pushenv " ++ name
writeInstruction (PushVar name) = "pushvar " ++ name
writeInstruction (ChangeVar name value) = "changevar " ++ name ++ " " ++ show value
writeInstruction Ret = "ret"

-- | Writes a program into a string.
-- >>> writeProgram [Push (Number 1), Push (Number 2), Add]
writeProgram :: Program -> String
writeProgram (x : xs) = writeInstruction x ++ "\n" ++ writeProgram xs
writeProgram [] = ""