module Language.RainML.Parser
  ( parseString
  ) where

import Data.Bifunctor
import Data.Char
import Data.Void

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

parseString :: String -> Either String Int
parseString xs = bimap parseErrorPretty id $ parse whileParser "<filename>" xs

type Parser = Parsec Void String

-- Space consumer.
sc :: Parser ()
sc = L.space space1 line block
  where
    line  = L.skipLineComment "--"
    block = L.skipBlockComment "{-" "-}"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

integer :: Parser Int
integer = lexeme L.decimal

rword :: String -> Parser ()
rword w = lexeme $ try $ string w *> notFollowedBy alphaNumChar

rws :: [String]
rws = []

identifier :: Parser String
identifier = lexeme $ try $ p >>= check
  where
    p = (:) <$> letterChar <*> many alphaNumChar
    check x =
      if x `elem` rws
        then fail $ "keyword " ++ show x ++ " is not an identifier"
        else return x

whileParser :: Parser Int
whileParser = between sc eof integer
