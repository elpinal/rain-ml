module Language.RainML.Syntax
  ( Term(..)
  ) where

data Term
  = Add Term Term
  | Int Int
  deriving (Eq, Show)
