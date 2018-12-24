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

parseString :: FilePath -> String -> Either String (S.Positional S.Term)
parseString fp xs = bimap errorBundlePretty id $ parse whileParser fp xs

type Parser = Parsec Void String

type Position = S.Position

-- Space consumer.
sc :: Parser ()
sc = L.space space1 line block
  where
    line  = L.skipLineComment "--"
    block = L.skipBlockComment "{-" "-}"

lexeme :: Parser a -> Parser (S.Positional a)
lexeme p = L.lexeme sc $ do
  start <- getSourcePos
  x <- p
  end <- getSourcePos
  return $ S.Positional (S.Position start end) x

symbol :: String -> Parser (S.Positional String)
symbol = lexeme . C.string

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

integer :: Parser (S.Positional Int)
integer = lexeme L.decimal

bool :: Parser (S.Positional Bool)
bool = flip S.Positional True <$> reserved "true"
   <|> flip S.Positional False <$> reserved "false"

term :: Parser (S.Positional S.Term)
term = makeExprParser p arithOperators
  where
    p            :: Parser (S.Positional S.Term)
    p            = fmap lit $ ffmap S.Bool bool <|> ffmap S.Int integer
    lit x        = S.Positional (S.getPosition x) $ S.Lit x
    ffmap        = fmap . fmap

arithOperators :: [[Operator Parser (S.Positional S.Term)]]
arithOperators = [[InfixL $ f S.Add <$ symbol "+"]]
  where
    f g x y = S.Positional
      { S.getPosition = S.Position (S.start $ S.getPosition x) (S.end $ S.getPosition y)
      , S.fromPositional = g x y
      }

-- Reserved words, that is, keywords.
reserved :: String -> Parser Position
reserved w = fmap S.getPosition $ lexeme $ try $ string w *> notFollowedBy alphaNumChar

reservedWords :: [String]
reservedWords =
  [ "true"
  , "false"
  ]

identifier :: Parser (S.Positional String)
identifier = lexeme $ try $ p >>= check
  where
    p = (:) <$> letterChar <*> many alphaNumChar
    check x =
      if x `elem` reservedWords
        then fail $ "keyword " ++ show x ++ " is not an identifier"
        else return x

whileParser :: Parser (S.Positional S.Term)
whileParser = between sc eof term
