{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}

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
import Language.RainML.Syntax (fromPositional)

data Type
  = IntType
  deriving (Eq, Show)

data Literal
  = Int Int
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
  | Arith ArithOp Int Value
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

fromLiteral :: S.Literal -> [Decl]
fromLiteral (S.Int n)  = [Id $ int n]
fromLiteral (S.Bool b) = [Id $ int $ fromEnum b]

toDecls :: S.Term -> [Decl]
toDecls (S.Lit l)     = fromLiteral $ fromPositional l
toDecls (S.Add t1 t2) =
  let xs = toDecls (fromPositional t1) in
  let ys = toDecls (fromPositional t2) in
  let (zs, u) =
        if length xs <= length ys
          then (xs ++ ys, length ys)
          else (ys ++ xs, length xs)
  in
    zs ++ [Arith Add 0 $ Var u]

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

instance Typing Value where
  typeOf (Lit l) = typeOf l
  typeOf (Var n) = ask >>= get n

instance Typing Decl where
  typeOf (Id v) = typeOf v
  typeOf (Arith Add n v2) = do
    typeOf (Var n) >>= expect IntType
    typeOf v2 >>= expect IntType
    return IntType

instance Typing Term where
  typeOf (Value v) = typeOf v
  typeOf (Let d t) = do
    ty <- typeOf d
    local (add ty) $ typeOf t

typecheck :: Term -> Either TypeError ()
typecheck t = run $ runError $ runReader (Context mempty) $ typeOf t >>= expect IntType
