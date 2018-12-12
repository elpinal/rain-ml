module Language.RainML.Intermediate
  ( translate
  , Value(..)
  , ArithOp(..)
  , Decl(..)
  , Term(..)
  ) where

import qualified Language.RainML.Syntax as S

data Value
  = Int Int
  | Var Int
  deriving (Eq, Show)

data ArithOp
  = Add
  deriving (Eq, Show)

data Decl
  = Id Value
  | Arith ArithOp Value Value
  deriving (Eq, Show)

data Term
  = Let Decl Term
  | Value Value
  deriving (Eq, Show)

var :: Int -> Term
var = Value . Var

translate :: S.Term -> Term
translate = foldr Let (var 0) . toDecls

toDecls :: S.Term -> [Decl]
toDecls (S.Int n)     = [Id $ Int n]
toDecls (S.Add t1 t2) = toDecls t1 ++ addLeft t2 -- Assume associativity.

addLeft :: S.Term -> [Decl]
addLeft (S.Int n)     = [Arith Add (Var 0) $ Int n]
addLeft (S.Add t1 t2) = addLeft t1 ++ addLeft t2
