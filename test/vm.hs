import Test.QuickCheck
import qualified System.Exit as Exit
import Test.HUnit
import VM (add, sub, mul, divi, eqq, less, nnot, nnot', jumpIfFalse, getargs, call, changevars, changevarslist, exec, Instruction(..), Value(..), Builtin(..))
import VMParserTests

testAddition :: Int -> Int -> Bool
testAddition a b  = case add [Number a, Number b] of
    Right [Number c] -> c == a + b
    _ -> False

testSoustraction :: Int -> Int -> Bool
testSoustraction a b  = case sub [Number a, Number b] of
    Right [Number c] -> c == a - b
    _ -> False

testMultiplication :: Int -> Int -> Bool
testMultiplication a b  = case mul [Number a, Number b] of
    Right [Number c] -> c == a * b
    _ -> False

testDivision :: Int -> Int -> Bool
testDivision a 0 = case divi [Number 0, Number a] of
    Left _ -> True
    _ -> False
testDivision a b  = case divi [Number b, Number a] of
    Right [Number c] -> c == a `Prelude.div` b
    _ -> False

testeqq :: Int -> Int -> Bool
testeqq a b  = case eqq [Number a, Number b] of
    Right [Boolean c] -> c == (a == b)
    _ -> False

testLess :: Int -> Int -> Bool
testLess a b  = case less [Number a, Number b] of
    Right [Boolean c] -> c == (b < a)
    _ -> False

testnotless :: Int -> Int -> Bool
testnotless a b  = case nnot Less [Number a, Number b] of
    Right [Boolean c] -> c == nnot' (a > b)
    _ -> False

testnoteqq :: Int -> Int -> Bool
testnoteqq a b  = case nnot Eqq [Number a, Number b] of
    Right [Boolean c] -> c == (a /= b)
    _ -> False

testjumpiffalse1 :: Test
testjumpiffalse1 = TestCase (assertEqual "should return Right [Push (Number 2)]" (jumpIfFalse 1 [Boolean False] [Push (Number 1), Push (Number 2)]) (Right [Push (Number 2)]))

testjumpiffalse2 :: Test
testjumpiffalse2 = TestCase (assertEqual "should return Right [Push (Number 1), Push (Number 2)]" (jumpIfFalse 2 [Boolean True] [Push (Number 1), Push (Number 2)]) (Right [Push (Number 1), Push (Number 2)]))

testjumpiffalse3 :: Test
testjumpiffalse3 = TestCase (assertEqual "should return Left 'Invalid arguments for jumpIfFalse'" (jumpIfFalse 1 [Number 1] [Push (Number 1), Push (Number 2)]) (Left "Invalid arguments for jumpIfFalse"))

testgetarg1 :: Test
testgetarg1 = TestCase (assertEqual "should return [Number 1, Number 2]" (getargs 2 [Number 1, Number 2]) [Number 1, Number 2])

testgetarg2 :: Test
testgetarg2 = TestCase (assertEqual "should return [Number 1, Number 2]" (getargs 2 [Number 1, Number 2, Number 3]) [Number 1, Number 2])

testgetarg3 :: Test
testgetarg3 = TestCase (assertEqual "should return [Number 1]" (getargs 2 [Number 1]) [Number 1])

testgetarg4 :: Test
testgetarg4 = TestCase (assertEqual "should return []" (getargs 0 [Number 1, Number 2, Number 3]) [])

testcall1 :: Test
testcall1 = TestCase (assertEqual "should return Right [Number 3]" (call [] [] (Builtin Add) [Number 1, Number 2]) (Right [Number 3]))

testcall2 :: Test
testcall2 = TestCase (assertEqual "should return Right [Number (-1)]" (call [] [] (Builtin Sub) [Number 1, Number 2]) (Right [Number (-1)]))

testcall3 :: Test
testcall3 = TestCase (assertEqual "should return Right [Number 2]" (call [] [] (Builtin Mul) [Number 1, Number 2]) (Right [Number 2]))

testcall4 :: Test
testcall4 = TestCase (assertEqual "should return Right [Number 2]" (call [] [] (Builtin Div) [Number 1, Number 2]) (Right [Number 2]))

testcall5 :: Test
testcall5 = TestCase (assertEqual "should return Left 'Division by zero'" (call [] [] (Builtin Div) [Number 0, Number 1]) (Left "Division by zero"))

testcall6 :: Test
testcall6 = TestCase (assertEqual "should return Right [Boolean False]" (call [] [] (Builtin Eqq) [Number 1, Number 2]) (Right [Boolean False]))

testcall7 :: Test
testcall7 = TestCase (assertEqual "should return Right [Boolean True]" (call [] [] (Builtin Eqq) [Number 1, Number 1]) (Right [Boolean True]))

testcall8 :: Test
testcall8 = TestCase (assertEqual "should return Right [Boolean False]" (call [] [] (Builtin Less) [Number 1, Number 2]) (Right [Boolean False]))

testcall9 :: Test
testcall9 = TestCase (assertEqual "should return Right [Boolean True]" (call [] [] (Builtin Less) [Number 2, Number 1]) (Right [Boolean True]))

testcall10 :: Test
testcall10 = TestCase (assertEqual "should return Right [Boolean False]" (call [] [] (Builtin Less) [Number 1, Number 1]) (Right [Boolean False]))

testcall11 :: Test
testcall11 = TestCase (assertEqual "should return Right [Boolean True]" (call [] [] (Builtin (Not Less)) [Number 1, Number 2]) (Right [Boolean True]))

testcall12 :: Test
testcall12 = TestCase (assertEqual "should return Right [Boolean False]" (call [] [] (Builtin (Not Less)) [Number 2, Number 1]) (Right [Boolean False]))

testcall13 :: Test
testcall13 = TestCase (assertEqual "should return Right [Boolean True]" (call [] [] (Builtin (Not Less)) [Number 1, Number 1]) (Right [Boolean True]))

testcall14 :: Test
testcall14 = TestCase (assertEqual "should return Right [Boolean True]" (call [] [] (Builtin (Not Eqq)) [Number 1, Number 2]) (Right [Boolean True]))

testcall15 :: Test
testcall15 = TestCase (assertEqual "should return Right [Boolean False]" (call [] [] (Builtin (Not Eqq)) [Number 1, Number 1]) (Right [Boolean False]))

testcall16 :: Test
testcall16 = TestCase (assertEqual "should return Right [Number 3, Number 2, Number 1]" (call [] [] (Function "test" 2 [PushArg 0, PushArg 1, Push (Builtin Add), Call, Ret]) [Number 2, Number 1]) (Right [Number 3, Number 2, Number 1]))

testcall17 :: Test
testcall17 = TestCase (assertEqual "should return Right [Boolean False, Number 2, Number 1]" (call [] [] (Function "test" 2 [PushArg 0, PushArg 1, Push (Builtin Less), Call, Ret]) [Number 2, Number 1]) (Right [Boolean False, Number 2, Number 1]))

testcall18 :: Test
testcall18 = TestCase (assertEqual "should return Left Right [Function 'test2' 0 [Push (Number 0), Ret], Number 2]" (call [] [("test2", (0, [Push (Number 0), Ret]))] (Function "test" 0 [PushEnv "test2", Ret]) [Number 2]) (Right [Function "test2" 0 [Push (Number 0), Ret], Number 2]))

testcall19 :: Test
testcall19 = TestCase (assertEqual "should return Right [Builtin Add, Number 2]" (call [] [] (Function "test" 0 [Push (Builtin Add),Ret]) [Number 2]) (Right [Builtin Add, Number 2]))

testcall20 :: Test
testcall20 = TestCase (assertEqual "should return Right [Chaine 'teststp', Number 2, Number 1]" (call [] [] (Function "test" 0 [Push (Chaine "teststp"), Ret]) [Number 2, Number 1]) (Right [Chaine "teststp", Number 2, Number 1]))

testcall21 :: Test
testcall21 = TestCase (assertEqual "should return Left 'Invalid call'" (call [] [] (Number 1) [Number 2, Number 1]) (Left "Invalid call"))

testcall22 :: Test
testcall22 = TestCase (assertEqual "should return  Left 'Invalid arguments for add'" (call [] [] (Function "test" 0 [Push (Builtin Add), Call,Ret]) [Number 2, Number 1]) (Left "Invalid arguments for add"))

testchangevars1 :: Test
testchangevars1 = TestCase (assertEqual "should return [('test', Number 3)]" (changevars [("test", Number 1)] "test" (Number 3)) [("test", Number 3)])

testchangevars2 :: Test
testchangevars2 = TestCase (assertEqual "should return [('test', Number 1)]" (changevars [("test", Number 1)] "test2" (Number 3)) [("test", Number 1)])

testchangevarslist1 :: Test
testchangevarslist1 = TestCase (assertEqual "should return [('test', Number 1)]" (changevarslist [("test", Number 1)] "test" (Number 3)) [("test", Number 3)])

testchangevarslist2 :: Test
testchangevarslist2 = TestCase (assertEqual "should return [('test2', Number 2), ('test', Number 1)]" (changevarslist [("test", Number 1)] "test2" (Number 2)) [("test2", Number 2), ("test", Number 1)])

testexec1 :: Test
testexec1 = TestCase (assertEqual "should return Left 'Invalid programe'" (exec [] []  [] [Push (Number 1), Push (Number 2), Push (Builtin Add)] []) (Left "Invalid programe"))

testexec2 :: Test
testexec2 = TestCase (assertEqual "should return Right [Number 3]" (exec [] []  [] [Push (Number 1), Push (Number 2), Push (Builtin Add), Call, Ret] []) (Right (Number 3)))

testexec3 :: Test
testexec3 = TestCase (assertEqual "should return Right [Number 1]" (exec [Number 1] []  [] [PushArg 0, Ret] []) (Right (Number 1)))

testexec4 :: Test
testexec4 = TestCase (assertEqual "should return Left 'Invalid env'" (exec [Number 1] []  [] [PushEnv "test", Ret] []) (Left "Invalid env"))

testexec5 :: Test
testexec5 = TestCase (assertEqual "should return Right (Function 'test' 0 [PushArg 0, Ret])" (exec [Number 1] [("test", (0, [PushArg 0, Ret]))]  [] [PushEnv "test", Ret] []) (Right (Function "test" 0 [PushArg 0, Ret])))

testexec6 :: Test
testexec6 = TestCase (assertEqual "should return Right (Number 5)" (exec [Number 1] [("test", (0, [PushArg 0, Ret]))]  [("test", Number 5)] [PushVar "test", Ret] []) (Right (Number 5)))

testexec7 :: Test
testexec7 = TestCase (assertEqual "should return Left 'Invalid push var'" (exec [Number 1] [("test", (0, [PushArg 0, Ret]))]  [] [PushVar "test", Ret] []) (Left "Invalid push var"))

testexec8 :: Test
testexec8 = TestCase (assertEqual "should return Right (Number 5)" (exec [Number 1] [("test", (0, [PushArg 0, Ret]))]  [("test", Number 5)] [ChangeVar "test" (Number 6), PushVar "test", Ret] []) (Right (Number 6)))

testexec9 :: Test
testexec9 = TestCase (assertEqual "should return Right (Number 5)" (exec [Number 1] [("test", (0, [PushArg 0, Ret]))]  [("test", Number 5)] [ChangeVar "test2" (Number 6), PushVar "test2", Ret] []) (Right (Number 6)))

testexec10 :: Test
testexec10 = TestCase (assertEqual "should return Right (Number 2)" (exec [Number 1] [("test", (0, [PushArg 0, Ret]))]  [("test", Number 5)] [Push (Boolean True), JumpIfFalse 2, Push (Number 2), Ret, Push (Number 6), Ret] []) (Right (Number 2)))

testexec11 :: Test
testexec11 = TestCase (assertEqual "should return Right (Number 6)" (exec [Number 1] [("test", (0, [PushArg 0, Ret]))]  [("test", Number 5)] [Push (Boolean False), JumpIfFalse 2, Push (Number 2), Ret, Push (Number 6), Ret] []) (Right (Number 6)))

testexec12 :: Test
testexec12 = TestCase (assertEqual "should return Left 'Invalid arguments for jumpIfFalse'" (exec [Number 1] [("test", (0, [PushArg 0, Ret]))]  [("test", Number 5)] [Push (Number 5), JumpIfFalse 2, Push (Number 2), Ret, Push (Number 6), Ret] []) (Left "Invalid arguments for jumpIfFalse"))

tests :: Test
tests = TestList
    [ TestLabel "jumpIfFalse with False" testjumpiffalse1
    , TestLabel "jumpIfFalse with True" testjumpiffalse2
    , TestLabel "jumpIfFalse with invalid arguments" testjumpiffalse3
    , TestLabel "get 2 args with 2 element in stack" testgetarg1
    , TestLabel "get 2 args with 3 element in stack" testgetarg2
    , TestLabel "get 2 args but i have just 1 element in stack" testgetarg3
    , TestLabel "get 0 args" testgetarg4
    , TestLabel "call builtin add" testcall1
    , TestLabel "call builtin sub" testcall2
    , TestLabel "call builtin mul" testcall3
    , TestLabel "call builtin div" testcall4
    , TestLabel "call builtin div by zero" testcall5
    , TestLabel "call builtin eqq false" testcall6
    , TestLabel "call builtin eqq true" testcall7
    , TestLabel "call builtin less false" testcall8
    , TestLabel "call builtin less true" testcall9
    , TestLabel "call builtin less eqq" testcall10
    , TestLabel "call builtin not less true" testcall11
    , TestLabel "call builtin not less false" testcall12
    , TestLabel "call builtin not less eqq" testcall13
    , TestLabel "call builtin not eqq true" testcall14
    , TestLabel "call builtin not eqq false" testcall15
    , TestLabel "call function return number" testcall16
    , TestLabel "call function return boolean" testcall17
    , TestLabel "call function return function" testcall18
    , TestLabel "call function return builtin builtin" testcall19
    , TestLabel "call function return chaine" testcall20
    , TestLabel "call with invalid arguments" testcall21
    , TestLabel "call function return error" testcall22
    , TestLabel "changevars change value" testchangevars1
    , TestLabel "changevars but vars doesn't in [vars]" testchangevars2
    , TestLabel "changevarslist change value" testchangevarslist1
    , TestLabel "changevarslist create vars" testchangevarslist2
    , TestLabel "exec with invalid programe" testexec1
    , TestLabel "exec with valid programe" testexec2
    , TestLabel "exec with valid programe and args" testexec3
    , TestLabel "exec with invalid env" testexec4
    , TestLabel "exec with valid env" testexec5
    , TestLabel "exec with valid vars" testexec6
    , TestLabel "exec with invalid push var" testexec7
    , TestLabel "exec with valid change var" testexec8
    , TestLabel "exec with create var" testexec9
    , TestLabel "exec with jump if false with True" testexec10
    , TestLabel "exec with jump if false with False" testexec11
    , TestLabel "exec with jump if false with invalid arguments" testexec12
    , TestLabel "my break with empty string" testMyBreakEmpty
    , TestLabel "my break with no correspondence" testMyBreakNoCorrespondence
    , TestLabel "my break with correspondence" testMyBreakCorrespondence
    , TestLabel "parse string to array with empty string" testParseStringToArrayEmpty
    , TestLabel "parse string to array with no correspondence" testParseStringToArrayNoCorrespondence
    , TestLabel "parse string to array with correspondence" testParseStringToArrayCorrespondence
    , TestLabel "parse string to array with some correspondence" testParseStringToArraySomeCorrespondence
    , TestLabel "start with empty string" testStartWithEmpty
    , TestLabel "start with no correspondence" testStartWithNoCorrespondence
    , TestLabel "start with correspondence" testStartWithCorrespondence
    , TestLabel "find main with empty array" testFindMainEmpty
    , TestLabel "find main with no correspondence" testFindMainNoCorrespondence
    , TestLabel "find main with correspondence" testFindMainCorrespondence
    , TestLabel "remove indemptation with empty string" testRemoveIndemptationEmpty
    , TestLabel "remove indemptation with no correspondence" testRemoveIndemptationNoCorrespondence
    , TestLabel "remove indemptation with correspondence" testRemoveIndemptationCorrespondence
    , TestLabel "cut first word with empty string" testCutFirstWordEmpty
    , TestLabel "cut first word with no correspondence" testCutFirstWordNoCorrespondence
    , TestLabel "cut first word with correspondence" testCutFirstWordCorrespondence
    , TestLabel "get end of string with empty string" testGetEndOfStringEmpty
    , TestLabel "get end of string with no correspondence" testGetEndOfStringNoCorrespondence
    , TestLabel "get end of string with correspondence" testGetEndOfStringCorrespondence
    , TestLabel "get string with empty string" testGetStringEmpty
    , TestLabel "get string with no correspondence" testGetStringNoCorrespondence
    , TestLabel "get string with correspondence" testGetStringCorrespondence
    , TestLabel "get number with empty string" testGetNumberEmpty
    , TestLabel "get number with no correspondence" testGetNumberNoCorrespondence
    , TestLabel "get number with correspondence" testGetNumberCorrespondence
    , TestLabel "get bool with true" testGetBoolTrue
    , TestLabel "get bool with false" testGetBoolFalse
    , TestLabel "get bool with no correspondence" testGetBoolNoCorrespondence
    , TestLabel "get bool with empty string" testGetBoolEmpty
    , TestLabel "get true with space" testGetBoolTrueWithSpace
    , TestLabel "get builtin with empty string" testGetBuiltinEmpty
    , TestLabel "get builtin with no correspondence" testGetBuiltinNoCorrespondence
    , TestLabel "get builtin add" testGetBuiltinAdd
    , TestLabel "get builtin sub" testGetBuiltinSub
    , TestLabel "get builtin mul" testGetBuiltinMul
    , TestLabel "get builtin div" testGetBuiltinDiv
    , TestLabel "get builtin eq" testGetBuiltinEq
    , TestLabel "get builtin less" testGetBuiltinLess
    , TestLabel "get builtin not" testGetBuiltinNot
    , TestLabel "get add with space" testGetBuiltinAddWithSpace
    , TestLabel "get sub with space" testGetBuiltinSubWithSpace
    , TestLabel "get mul with space" testGetBuiltinMulWithSpace
    , TestLabel "get div with space" testGetBuiltinDivWithSpace
    , TestLabel "get eq with space" testGetBuiltinEqWithSpace
    , TestLabel "get less with space" testGetBuiltinLessWithSpace
    , TestLabel "get not with space" testGetBuiltinNotWithSpace
    , TestLabel "get not with error" testGetBuiltinNotError
    , TestLabel "rest to value with string" testRestToValueString
    , TestLabel "rest to value with number" testRestToValueNumber
    , TestLabel "rest to value with boolean" testRestToValueBool
    , TestLabel "rest to value with builtin" testRestToValueBuiltin
    , TestLabel "rest to value error" testRestToValueEmpty
    , TestLabel "rest to value error" testRestToValueNoCorrespondence
    ]

main :: IO ()
main = do
    putStrLn "\nQuickCheck for all builtin:"
    quickCheck testAddition
    quickCheck testSoustraction
    quickCheck testMultiplication
    quickCheck testDivision
    quickCheck testeqq
    quickCheck testLess
    quickCheck testnotless
    quickCheck testnoteqq
    putStrLn "\nHUnit for all functions:"
    result <- runTestTT tests
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess
