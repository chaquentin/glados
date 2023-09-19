module Parser () where

import Control.Applicative (Alternative (some))
import Data.Char (isDigit)
import DataTypes (Expression (..))

valueParser :: t
valueParser =
  nullParser
    <|> boolParser
    <|> numberParser
    <|> stringParser