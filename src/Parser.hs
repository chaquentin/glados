{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Parser
-- Description : The parser for the language.
module Parser
  ( -- * Parsing
    parseAst,
  )
where

import Control.Applicative
import Control.Monad
import Data.Char
import DataTypes (Ast (..), BinaryOperator (..), Literal (..), UnaryOperator (..))

newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}

instance Functor Parser where
  fmap f (Parser p) = Parser $ \input -> do
    (x, input') <- p input
    Just (f x, input')

instance Applicative Parser where
  pure x = Parser $ \input -> Just (x, input)
  (Parser p1) <*> (Parser p2) = Parser $ \input -> do
    (f, input') <- p1 input
    (x, input'') <- p2 input'
    Just (f x, input'')

instance Alternative Parser where
  empty = Parser $ const Nothing
  (Parser p1) <|> (Parser p2) = Parser $ \input -> p1 input <|> p2 input

instance Monad Parser where
  return = pure
  (Parser p) >>= f = Parser $ \input -> do
    (x, input') <- p input
    runParser (f x) input'

instance MonadPlus Parser where
  mzero = empty
  mplus = (<|>)

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \case
  c : cs | f c -> Just (c, cs)
  _ -> Nothing

char :: Char -> Parser Char
char c = satisfy (== c)

string :: String -> Parser String
string = traverse char

digit :: Parser Char
digit = satisfy isDigit

natural :: Parser Integer
natural = read <$> some digit

space :: Parser Char
space = satisfy isSpace

spaces :: Parser String
spaces = many space

token :: Parser a -> Parser a
token p = p <* spaces

symbol :: String -> Parser String
symbol = token . string

parens :: Parser a -> Parser a
parens p = symbol "(" *> p <* symbol ")"

reservedOp :: String -> Parser String
reservedOp = token . string

identifier :: Parser String
identifier = (:) <$> satisfy isAlpha <*> many (satisfy isAlphaNum)

literal :: Parser Literal
literal =
  Number . fromIntegral <$> natural
    <|> String <$> (char '"' *> many (satisfy (/= '"')) <* char '"')
    <|> Boolean True <$ string "true"
    <|> Boolean False <$ string "false"
    <|> Nil <$ string "nil"

binaryOperator :: Parser BinaryOperator
binaryOperator =
  Add <$ reservedOp "+"
    <|> Subtract <$ reservedOp "-"
    <|> Multiply <$ reservedOp "*"
    <|> Divide <$ reservedOp "/"
    <|> Modulo <$ reservedOp "%"
    <|> Equal <$ reservedOp "=="
    <|> NotEqual <$ reservedOp "!="
    <|> LessThan <$ reservedOp "<"
    <|> GreaterThan <$ reservedOp ">"
    <|> LessThanOrEqual <$ reservedOp "<="
    <|> GreaterThanOrEqual <$ reservedOp ">="
    <|> And <$ reservedOp "and"
    <|> Or <$ reservedOp "or"

unaryOperator :: Parser UnaryOperator
unaryOperator =
  Negate <$ reservedOp "-"
    <|> Not <$ reservedOp "not"

ast :: Parser Ast
ast =
  functionDeclaration
    <|> varDeclaration
    <|> returnStatement
    <|> ifStatement
    <|> functionCall
    <|> assignment
    <|> UnaryExpression <$> unaryOperator <*> ast
    <|> binaryExpression
    <|> Literal <$> literal
    <|> Identifier <$> identifier
    <|> parens ast

binaryExpression :: Parser Ast
binaryExpression = BinaryExpression <$> binaryOperator <*> ast <*> ast

sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 p sep = (:) <$> p <*> many (sep *> p)

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p sep = sepBy1 p sep <|> pure []

functionDeclaration :: Parser Ast
functionDeclaration =
  FunctionDeclaration
    <$> (symbol "function" *> identifier)
    <*> parens (identifier `sepBy` reservedOp ",")
    <*> (reservedOp "{" *> many ast <* reservedOp "}")

varDeclaration :: Parser Ast
varDeclaration = VarDeclaration <$> (symbol "var" *> identifier <* reservedOp "=") <*> ast <* symbol ";"

returnStatement :: Parser Ast
returnStatement = ReturnStatement <$> (symbol "return" *> ast <* symbol ";")

ifStatement :: Parser Ast
ifStatement = IfStatement <$> (symbol "if" *> ast) <*> (reservedOp "{" *> many ast <* reservedOp "}")

functionCall :: Parser Ast
functionCall = FunctionCall <$> identifier <*> parens (ast `sepBy` reservedOp ",")

assignment :: Parser Ast
assignment = Assignment <$> (identifier <* reservedOp "=") <*> ast

parseAst :: String -> Maybe Ast
parseAst = fmap fst . runParser (spaces *> ast)