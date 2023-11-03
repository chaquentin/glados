module VMParserTests where

import Test.HUnit
import ParseBin
import VM

testMyBreakEmpty :: Test
testMyBreakEmpty = TestCase (assertEqual "should return tuple of two empty arrays" ([], []) (myBreak ""))

testMyBreakNoCorrespondence :: Test
testMyBreakNoCorrespondence = TestCase (assertEqual "should return tuple of an empty array and the string passed " (myBreak "test") ("test", []))

testMyBreakCorrespondence :: Test
testMyBreakCorrespondence = TestCase (assertEqual "should return tuple of an array with the first element and the rest of the string" (myBreak "test\ntest") ("test", "test"))

testParseStringToArrayEmpty :: Test
testParseStringToArrayEmpty = TestCase (assertEqual "should return an empty array" (parseStringToArray "") [""])

testParseStringToArrayNoCorrespondence :: Test
testParseStringToArrayNoCorrespondence = TestCase (assertEqual "should return an array with the string passed" (parseStringToArray "test") ["test"])

testParseStringToArrayCorrespondence :: Test
testParseStringToArrayCorrespondence = TestCase (assertEqual "should return an array with the two strings passed" (parseStringToArray "test\ntest") ["test", "test"])

testParseStringToArraySomeCorrespondence :: Test
testParseStringToArraySomeCorrespondence = TestCase (assertEqual "should return an array with the two strings passed" (parseStringToArray "test\ntest\ntest") ["test", "test", "test"])

testStartWithEmpty :: Test
testStartWithEmpty = TestCase (assertEqual "should return true" (startWith "" "") True)

testStartWithNoCorrespondence :: Test
testStartWithNoCorrespondence = TestCase (assertEqual "should return false" (startWith "test" "") False)

testStartWithCorrespondence :: Test
testStartWithCorrespondence = TestCase (assertEqual "should return true" (startWith "test" "test") True)

testFindMainEmpty :: Test
testFindMainEmpty = TestCase (assertEqual "should return an error" (findMain []) (Left "Error: need a main function"))

testFindMainNoCorrespondence :: Test
testFindMainNoCorrespondence = TestCase (assertEqual "should return an error" (findMain ["test"]) (Left "Error: need a main function"))

testFindMainCorrespondence :: Test
testFindMainCorrespondence = TestCase (assertEqual "should return the rest of the array" (findMain ["main:", "test"]) (Right ["test"]))

testRemoveIndemptationEmpty :: Test
testRemoveIndemptationEmpty = TestCase (assertEqual "should return an empty string" (removeIndemptation "") "")

testRemoveIndemptationNoCorrespondence :: Test
testRemoveIndemptationNoCorrespondence = TestCase (assertEqual "should return the string passed" (removeIndemptation "test") "test")

testRemoveIndemptationCorrespondence :: Test
testRemoveIndemptationCorrespondence = TestCase (assertEqual "should return the string passed" (removeIndemptation "  \t             test") "test")

testCutFirstWordEmpty :: Test
testCutFirstWordEmpty = TestCase (assertEqual "should return an empty string" (cutFirstWord "") ("", ""))

testCutFirstWordNoCorrespondence :: Test
testCutFirstWordNoCorrespondence = TestCase (assertEqual "should return the string passed" (cutFirstWord "test") ("test", ""))

testCutFirstWordCorrespondence :: Test
testCutFirstWordCorrespondence = TestCase (assertEqual "should return the string passed" (cutFirstWord "test            \tokfd") ("test", "okfd"))

testGetEndOfStringEmpty :: Test
testGetEndOfStringEmpty = TestCase (assertEqual "should return an error" (getEndOfString "") (Left "Error: no end of string"))

testGetEndOfStringNoCorrespondence :: Test
testGetEndOfStringNoCorrespondence = TestCase (assertEqual "should return an error" (getEndOfString "test") (Left "Error: no end of string"))

testGetEndOfStringCorrespondence :: Test
testGetEndOfStringCorrespondence = TestCase (assertEqual "should return the string passed" (getEndOfString "\"test\"") (Right ("", "test\"")))

testGetStringEmpty :: Test
testGetStringEmpty = TestCase (assertEqual "should return an error" (getString "") (Left "Error: no start of string"))

testGetStringNoCorrespondence :: Test
testGetStringNoCorrespondence = TestCase (assertEqual "should return an error" (getString "test") (Left "Error: no start of string"))

testGetStringCorrespondence :: Test
testGetStringCorrespondence = TestCase (assertEqual "should return the string passed" (getString "\"test\"") (Right ("test", "")))

testGetNumberEmpty :: Test
testGetNumberEmpty = TestCase (assertEqual "should return an empty string" (getNumber "") ("", ""))

testGetNumberNoCorrespondence :: Test
testGetNumberNoCorrespondence = TestCase (assertEqual "should return an empty string" (getNumber "test") ("", "test"))

testGetNumberCorrespondence :: Test
testGetNumberCorrespondence = TestCase (assertEqual "should return the string passed" (getNumber "123test") ("123", "test"))

testGetBoolEmpty :: Test
testGetBoolEmpty = TestCase (assertEqual "should return an error" (getBool "") (Left "Error: not a boolean"))

testGetBoolNoCorrespondence :: Test
testGetBoolNoCorrespondence = TestCase (assertEqual "should return an error" (getBool "test") (Left "Error: not a boolean"))

testGetBoolTrue :: Test
testGetBoolTrue = TestCase (assertEqual "should return true" (getBool "true") (Right True))

testGetBoolFalse :: Test
testGetBoolFalse = TestCase (assertEqual "should return false" (getBool "false") (Right False))

testGetBoolTrueWithSpace :: Test
testGetBoolTrueWithSpace = TestCase (assertEqual "should return true" (getBool "  \ttrue        ") (Right True))

testGetBuiltinEmpty :: Test
testGetBuiltinEmpty = TestCase (assertEqual "should return an error" (getBuiltin "") (Left "Error: not a builtin"))

testGetBuiltinNoCorrespondence :: Test
testGetBuiltinNoCorrespondence = TestCase (assertEqual "should return an error" (getBuiltin "test") (Left "Error: not a builtin"))

testGetBuiltinAdd :: Test
testGetBuiltinAdd = TestCase (assertEqual "should return add" (getBuiltin "add") (Right Add))

testGetBuiltinAddWithSpace :: Test
testGetBuiltinAddWithSpace = TestCase (assertEqual "should return add" (getBuiltin "  \tadd        ") (Right Add))

testGetBuiltinSub :: Test
testGetBuiltinSub = TestCase (assertEqual "should return sub" (getBuiltin "sub") (Right Sub))

testGetBuiltinSubWithSpace :: Test
testGetBuiltinSubWithSpace = TestCase (assertEqual "should return sub" (getBuiltin "  \tsub        ") (Right Sub))

testGetBuiltinMul :: Test
testGetBuiltinMul = TestCase (assertEqual "should return mul" (getBuiltin "mul") (Right Mul))

testGetBuiltinMulWithSpace :: Test
testGetBuiltinMulWithSpace = TestCase (assertEqual "should return mul" (getBuiltin "  \tmul        ") (Right Mul))

testGetBuiltinDiv :: Test
testGetBuiltinDiv = TestCase (assertEqual "should return div" (getBuiltin "div") (Right Div))

testGetBuiltinDivWithSpace :: Test
testGetBuiltinDivWithSpace = TestCase (assertEqual "should return div" (getBuiltin "  \tdiv        ") (Right Div))

testGetBuiltinEq :: Test
testGetBuiltinEq = TestCase (assertEqual "should return eq" (getBuiltin "eq") (Right Eqq))

testGetBuiltinEqWithSpace :: Test
testGetBuiltinEqWithSpace = TestCase (assertEqual "should return eq" (getBuiltin "  \teq        ") (Right Eqq))

testGetBuiltinLess :: Test
testGetBuiltinLess = TestCase (assertEqual "should return less" (getBuiltin "less") (Right Less))

testGetBuiltinLessWithSpace :: Test
testGetBuiltinLessWithSpace = TestCase (assertEqual "should return less" (getBuiltin "  \tless        ") (Right Less))

testGetBuiltinNot :: Test
testGetBuiltinNot = TestCase (assertEqual "should return not" (getBuiltin "not add") (Right (Not Add)))

testGetBuiltinNotWithSpace :: Test
testGetBuiltinNotWithSpace = TestCase (assertEqual "should return not" (getBuiltin "  \tnot        add \t   ") (Right (Not  Add)))

testGetBuiltinNotError :: Test
testGetBuiltinNotError = TestCase (assertEqual "should return an error" (getBuiltin "not test") (Left "Error: not a builtin"))

testRestToValueEmpty :: Test
testRestToValueEmpty = TestCase (assertEqual "should return an error" (restToValue "") (Left "Error: not a builtin"))

testRestToValueNoCorrespondence :: Test
testRestToValueNoCorrespondence = TestCase (assertEqual "should return an error" (restToValue "test") (Left "Error: not a builtin"))

testRestToValueString :: Test
testRestToValueString = TestCase (assertEqual "should return a string" (restToValue "\"test\"") (Right (Chaine "test")))

testRestToValueNumber :: Test
testRestToValueNumber = TestCase (assertEqual "should return a number" (restToValue "123") (Right (Number 123)))

testRestToValueBool :: Test
testRestToValueBool = TestCase (assertEqual "should return a boolean" (restToValue "true") (Right (Boolean True)))

testRestToValueBuiltin :: Test
testRestToValueBuiltin = TestCase (assertEqual "should return a builtin" (restToValue "add") (Right (Builtin Add)))
