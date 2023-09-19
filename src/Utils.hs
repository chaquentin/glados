module Utils () where

import DataTypes (Expression (..))

isNull :: [Expression] -> Expression
isNull [Null] = Boolean True
isNull _ = Boolean False

isNumber :: [Expression] -> Expression
isNumber [Number _] = Boolean True
isNumber _ = Boolean False

isBoolean :: [Expression] -> Expression
isBoolean [Boolean _] = Boolean True
isBoolean _ = Boolean False

isVariable :: [Expression] -> Expression
isVariable [Variable _] = Boolean True
isVariable _ = Boolean False

isPair :: [Expression] -> Expression
isPair [Pair _ _] = Boolean True
isPair _ = Boolean False

numberEquals :: [Expression] -> Expression
numberEquals [Number n1, Number n2]
  | abs (n1 - n2) < 1e-6 = Boolean True
  | otherwise = Boolean False
numberEquals _ = Boolean False

booleanEquals :: [Expression] -> Expression
booleanEquals [Boolean b1, Boolean b2] = Boolean (b1 == b2)
booleanEquals _ = Boolean False

variableEquals :: [Expression] -> Expression
variableEquals [Variable v1, Variable v2] = Boolean (v1 == v2)
variableEquals _ = Boolean False

numberLessThan :: [Expression] -> Expression
numberLessThan [Number num1, Number num2] = Boolean (num1 < num2)
numberLessThan _ = Exception "Can't compare non-numbers."

numberGreaterThan :: [Expression] -> Expression
numberGreaterThan [Number num1, Number num2] = Boolean (num1 > num2)
numberGreaterThan _ = Exception "Can't compare non-numbers."

addNumbers :: [Expression] -> Expression
addNumbers [Number num1, Number num2] = Number (num1 + num2)
addNumbers _ = Exception "Can only add two numbers"

subtractNumbers :: [Expression] -> Expression
subtractNumbers [Number num1, Number num2] = Number (num1 - num2)
subtractNumbers _ = Exception "Can only subtract 2 numbers"

multiplyNumbers :: [Expression] -> Expression
multiplyNumbers [Number num1, Number num2] = Number (num1 * num2)
multiplyNumbers _ = Exception "Can only add two numbers"

notBoolean :: [Expression] -> Expression
notBoolean [Boolean b] = Boolean (not b)
notBoolean _ = Exception "Not one boolean"

andBool :: [Expression] -> Expression
andBool [Boolean b1, Boolean b2] = Boolean (b1 && b2)
andBool _ = Exception "Not two boolean"

orBool :: [Expression] -> Expression
orBool [Boolean b1, Boolean b2] = Boolean (b1 || b2)
orBool _ = Exception "Not tow boolean"

createPair :: [Expression] -> Expression
createPair [first, second] = Pair first second
createPair _ = Exception "A pair can only be created from 2 values"

pairFst :: [Expression] -> Expression
pairFst [Pair first _] = first
pairFst _ = Exception "Not a pair"

pairSnd :: [Expression] -> Expression
pairSnd [Pair _ second] = second
pairSnd _ = Exception "Not a pair"

createList :: [Expression] -> Expression
createList = foldr Pair Null

primitives :: String -> Expression
primitives "null" = Null
primitives "null?" = PrimitiveProcedure isNull
primitives "number?" = PrimitiveProcedure isNumber
primitives "boolean?" = PrimitiveProcedure isBoolean
primitives "variable?" = PrimitiveProcedure isVariable
primitives "pair?" = PrimitiveProcedure isPair
primitives "number-equals?" = PrimitiveProcedure numberEquals
primitives "boolean-equals?" = PrimitiveProcedure booleanEquals
primitives "variable-equals?" = PrimitiveProcedure variableEquals
primitives "list" = PrimitiveProcedure createList
primitives "not" = PrimitiveProcedure notBoolean
primitives "and" = PrimitiveProcedure andBool
primitives "or" = PrimitiveProcedure orBool
primitives "cons" = PrimitiveProcedure createPair
primitives "car" = PrimitiveProcedure pairFst
primitives "cdr" = PrimitiveProcedure pairSnd
primitives "+" = PrimitiveProcedure addNumbers
primitives "-" = PrimitiveProcedure subtractNumbers
primitives "*" = PrimitiveProcedure multiplyNumbers
primitives "<" = PrimitiveProcedure numberLessThan
primitives _ = Exception "Not a valid expression"