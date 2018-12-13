module Language.RainML.Parser
  ( parseString
  ) where

import Data.Bifunctor
import Data.Void

import Control.Monad.Combinators.Expr
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

import qualified Language.RainML.Syntax as S

parseString :: FilePath -> String -> Either String S.Term
parseString fp xs = bimap parseErrorPretty id $ parse whileParser fp xs

type Parser = Parsec Void String

type Position = S.Position

-- Space consumer.
sc :: Parser ()
sc = L.space space1 line block
  where
    line  = L.skipLineComment "--"
    block = L.skipBlockComment "{-" "-}"

lexeme :: Parser a -> Parser (a, Position)
lexeme p = L.lexeme sc $ do
  start <- getPosition
  x <- p
  end <- getPosition
  return (x, S.Position start end)

symbol :: String -> Parser (String, Position)
symbol = lexeme . C.string

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

integer :: Parser (Int, Position)
integer = lexeme L.decimal

bool :: Parser (Bool, Position)
bool = (,) True <$> reserved "true"
   <|> (,) False <$> reserved "false"

term :: Parser S.Term
term = makeExprParser (fst <$> p) arithOperators
  where
    p            = fmap lit $ f S.Bool <$> bool <|> f S.Int <$> integer
    f g (x, pos) = (g pos x, pos)
    lit          = f S.Lit

arithOperators :: [[Operator Parser S.Term]]
arithOperators = [[InfixL $ f S.Add <$> symbol "+"]]
  where
    f g (_, pos) = g pos

-- Reserved words, that is, keywords.
reserved :: String -> Parser Position
reserved w = fmap snd $ lexeme $ try $ string w *> notFollowedBy alphaNumChar

reservedWords :: [String]
reservedWords =
  [ "true"
  , "false"
  ]

identifier :: Parser (String, Position)
identifier = lexeme $ try $ p >>= check
  where
    p = (:) <$> letterChar <*> many alphaNumChar
    check x =
      if x `elem` reservedWords
        then fail $ "keyword " ++ show x ++ " is not an identifier"
        else return x

whileParser :: Parser S.Term
whileParser = between sc eof term
