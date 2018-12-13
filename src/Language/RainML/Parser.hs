module Language.RainML.Parser
  ( parseString
  ) where

import Data.Bifunctor
import Data.Char
import Data.Void

import Control.Monad.Combinators.Expr
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L


import qualified Language.RainML.Syntax as S

parseString :: FilePath -> String -> Either String S.Term
parseString fp xs = bimap parseErrorPretty id $ parse whileParser fp xs

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

arith :: Parser S.Term
arith = makeExprParser (S.Lit . S.Int <$> integer) arithOperators

arithOperators :: [[Operator Parser S.Term]]
arithOperators = [[InfixL $ S.Add <$ symbol "+"]]

rword :: String -> Parser ()
rword w = lexeme $ try $ string w *> notFollowedBy alphaNumChar

rws :: [String]
rws =
  [ "true"
  , "false"
  ]

identifier :: Parser String
identifier = lexeme $ try $ p >>= check
  where
    p = (:) <$> letterChar <*> many alphaNumChar
    check x =
      if x `elem` rws
        then fail $ "keyword " ++ show x ++ " is not an identifier"
        else return x

bool :: Parser S.Literal
bool = S.Bool True <$ rword "true"
   <|> S.Bool False <$ rword "false"

term :: Parser S.Term
term = arith <|> S.Lit <$> bool

whileParser :: Parser S.Term
whileParser = between sc eof term
