import Test.HUnit
import qualified System.Exit as Exit
import System.IO
import Parser (parseAst)
import DataTypes (Ast (..))
import Execute (printExecute)

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
    (output, _) <- capture_ $ printExecute (parseAst "(+ 2 3)")
    assertEqual "should return Number 5" (output) (Number 5)

tests :: Test
tests = TestList
    [ TestLabel "Basic Parser test with number" test1
    , TestLabel "Basic Parser test with variable" test2
    , TestLabel "Basic Parser test with bool" test3
    , TestLabel "Parsing define variable" test4
    , TestLabel "Basic execute test" test5
    ]

main :: IO ()
main = do
    result <- runTestTT tests
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess
