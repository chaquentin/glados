-- Add lambacase
{-# LANGUAGE LambdaCase #-}

module Parser
  ( parseAst,
  )
where

import Control.Applicative (Alternative (..), empty)
import DataTypes (Ast (..))

newtype Parser a = Parser (String -> [(a, String)])

instance Functor Parser where
  fmap f (Parser p) = Parser $ \input -> [(f a, b) | (a, b) <- p input]

instance Applicative Parser where
  pure a = Parser $ \input -> [(a, input)]
  Parser p1 <*> Parser p2 = Parser $ \input -> [(f a, input2) | (f, input1) <- p1 input, (a, input2) <- p2 input1]

instance Monad Parser where
  return = pure
  Parser p >>= f = Parser $ \input -> [(b, input2) | (a, input1) <- p input, (b, input2) <- parse (f a) input1]

instance Alternative Parser where
  empty = Parser $ const []
  Parser p1 <|> Parser p2 = Parser $ \input -> p1 input ++ p2 input

instance (Semigroup a) => Semigroup (Parser a) where
  Parser p1 <> Parser p2 = Parser $ \input -> [(a1 <> a2, input2) | (a1, input1) <- p1 input, (a2, input2) <- p2 input1]

instance (Monoid a) => Monoid (Parser a) where
  mempty = Parser $ const []

instance MonadFail Parser where
  fail _ = empty

parse :: Parser a -> String -> [(a, String)]
parse (Parser p) = p

item :: Parser Char
item = Parser $ \case
  [] -> []
  (x : xs) -> [(x, xs)]

sat :: (Char -> Bool) -> Parser Char
sat p = item >>= \x -> if p x then return x else empty

char :: Char -> Parser Char
char = sat . (==)

string :: String -> Parser String
string = traverse char

digit :: Parser Char
digit = sat (`elem` ['0' .. '9'])

number :: Parser Ast
number = Number . read <$> some digit

space :: Parser Char
space = sat (`elem` [' ', '\n', '\t'])

spaces :: Parser String
spaces = many space

token :: Parser a -> Parser a
token p = spaces *> p <* spaces

symbol :: String -> Parser String
symbol = token . string

parens :: Parser a -> Parser a
parens p = symbol "(" *> p <* symbol ")"

list :: Parser Ast
list = List <$> parens (many ast)

lambda :: Parser Ast
lambda = Lambda <$> (symbol "lambda" *> parens (many (token (some (sat (/= ' ')))))) <*> ast

if' :: Parser Ast
if' = If <$> (symbol "if" *> ast) <*> ast <*> ast

define :: Parser Ast
define = Define <$> (symbol "define" *> token (some (sat (/= ' ')))) <*> ast

variable :: Parser Ast
variable = Variable <$> token (some (sat (/= ' ')))

null' :: Parser Ast
null' = Null <$ symbol "null"

ast :: Parser Ast
ast = number <|> list <|> lambda <|> if' <|> define <|> variable <|> null'

parseAst :: String -> Ast
parseAst input = case parse ast input of
  [(a, [])] -> a
  [(_, out)] -> error $ "Unused input: " ++ out
  [] -> error "Invalid input"
  _ -> error "Ambiguous input"
