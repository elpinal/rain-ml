{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.RainML.Intermediate
  ( translate
  , Literal(..)
  , Value(..)
  , ArithOp(..)
  , Decl(..)
  , Term(..)

  , typecheck
  , TypeError(..)
  , Type(..)
  ) where

import Control.Monad.Freer hiding (translate)
import Control.Monad.Freer.Error
import Control.Monad.Freer.Reader

import qualified Language.RainML.Syntax as S

data Type
  = IntType
  | BoolType
  deriving (Eq, Show)

data Literal
  = Int Int
  | Bool Bool
  deriving (Eq, Show)

data Value
  = Lit Literal
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

int :: Int -> Value
int = Lit . Int

translate :: S.Term -> Term
translate = foldr Let (var 0) . toDecls

toDecls :: S.Term -> [Decl]
toDecls (S.Int n)     = [Id $ int n]
toDecls (S.Add t1 t2) = toDecls t1 ++ addLeft t2 -- Assume associativity.

addLeft :: S.Term -> [Decl]
addLeft (S.Int n)     = [Arith Add (Var 0) $ int n]
addLeft (S.Add t1 t2) = addLeft t1 ++ addLeft t2

data TypeError
  = TypeMismatch Type Type
  | UnboundVariable Int
  deriving (Eq, Show)

newtype Context = Context [Type]
  deriving (Eq, Show, Semigroup, Monoid)

get :: Member (Error TypeError) r => Int -> Context -> Eff r Type
get n (Context xs)
  | 0 <= n && n < length xs = return $ xs !! n
  | otherwise               = throwError $ UnboundVariable n

add :: Type -> Context -> Context
add ty (Context xs) = Context $ ty : xs

-- Type equality.
equal :: Type -> Type -> Bool
equal = (==)

expect :: Member (Error TypeError) r => Type -> Type -> Eff r ()
expect t1 t2
  | equal t1 t2 = return ()
  | otherwise   = throwError $ TypeMismatch t1 t2

class Typing a where
  typeOf :: Members [Reader Context, Error TypeError] r => a -> Eff r Type

instance Typing Literal where
  typeOf (Int _)  = return IntType
  typeOf (Bool _) = return BoolType

instance Typing Value where
  typeOf (Lit l) = typeOf l
  typeOf (Var n) = ask >>= get n

instance Typing Decl where
  typeOf (Id v) = typeOf v
  typeOf (Arith Add v1 v2) = do
    typeOf v1 >>= expect IntType
    typeOf v2 >>= expect IntType
    return IntType

instance Typing Term where
  typeOf (Value v) = typeOf v
  typeOf (Let d t) = do
    ty <- typeOf d
    local (add ty) $ typeOf t

typecheck :: Term -> Either TypeError ()
typecheck t = run $ runError $ runReader (Context mempty) $ typeOf t >>= expect IntType
