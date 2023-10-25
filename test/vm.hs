import Test.QuickCheck
import qualified System.Exit as Exit
import VM

testAddition :: Int -> Int -> Bool
testAddition a b  = case add [Number a, Number b] of
    Right [Number c] -> c == a + b
    _ -> False

testSoustraction :: Int -> Int -> Bool
testSoustraction a b  = case sub [Number a, Number b] of
    Right [Number c] -> c == b - a
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

testLess :: Int -> Int -> Bool
testLess a b  = case less [Number a, Number b] of
    Right [Boolean c] -> c == (b < a)
    _ -> False

main :: IO ()
main = do
    quickCheck testAddition
    quickCheck testSoustraction
    quickCheck testMultiplication
    quickCheck testDivision
    quickCheck testLess
