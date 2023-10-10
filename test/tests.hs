import Test.HUnit
import qualified System.Exit as Exit
import System.IO
import Parser (parseAst)
import DataTypes (Ast (..))
import Execute (buildInExecute)

test1 :: Test
test1 = TestCase (assertEqual "should return Number 3" (parseAst "3") (Number 3))

test2 :: Test
test2 = TestCase (assertEqual "should return Variable A" (parseAst "A") (Variable "A"))

test3 :: Test
test3 = TestCase (assertEqual "should return List(Number 1, Number 2)" (parseAst "(1 2)") (List[Number 1, Number 2]))

test4 :: Test
test4 = TestCase (assertEqual "should return List [Define 'ok' (Number 3)]" (parseAst ("(define ok 3)")) (List [Define "ok" (Number 3)]))

test5 :: Test
test5 = TestCase $ do
    ast <- buildInExecute (parseAst "(+ 1 3)")
    assertEqual "should return sum of 1 and 3" ast (Number 4)

testInfTrue :: Test
testInfTrue = TestCase $ do
    ast <- buildInExecute (parseAst "(< 2 3)")
    assertEqual "should return True" ast (Boolean True)

testInfFalse :: Test
testInfFalse = TestCase $ do
    ast <- buildInExecute (parseAst "(< 3 2)")
    assertEqual "should return False" ast (Boolean False)

testBasicNumber :: Test
testBasicNumber = TestCase $ do
    ast <- buildInExecute (parseAst "3")
    assertEqual "should return Number 3" ast (Number 3)

testMinus :: Test
testMinus = TestCase $ do
    ast <- buildInExecute (parseAst "(- 3 2)")
    assertEqual "should return Number 1" ast (Number 1)

testMinusNeg :: Test
testMinusNeg = TestCase $ do
    ast <- buildInExecute (parseAst "(- 3)")
    assertEqual "should return Number -3" ast (Number (-3))

testLongMinus :: Test
testLongMinus = TestCase $ do
    ast <- buildInExecute (parseAst "(- 3 2 1)")
    assertEqual "should return Number 0" ast (Number 0)

tests :: Test
tests = TestList
    [ TestLabel "Basic Parser test with number" test1
    , TestLabel "Basic Parser test with variable" test2
    , TestLabel "Basic Parser test with bool" test3
    , TestLabel "Parsing define variable" test4
    , TestLabel "Basic execute test" test5
    , TestLabel "Basic execute test with <" testInfTrue
    , TestLabel "Basic execute test with <" testInfFalse
    , TestLabel "Basic execute test with number" testBasicNumber
    , TestLabel "Basic execute test with minus" testMinus
    , TestLabel "Basic execute test with minus neg" testMinusNeg
    , TestLabel "Basic execute test with long minus" testLongMinus
    ]

main :: IO ()
main = do
    result <- runTestTT tests
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess
