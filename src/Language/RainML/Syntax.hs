module Language.RainML.Syntax
  ( Term(..)
  , Type(..)
  , TypeError(..)
  , typeOf
  , expect
  ) where

data Type
  = IntType
  deriving (Eq, Show)

data Term
  = Add Term Term
  | Int Int
  deriving (Eq, Show)

data TypeError
  = TypeMismatch Type Type
  deriving (Eq, Show)

-- Type equality.
equal :: Type -> Type -> Bool
equal = (==)

expect :: Type -> Type -> Either TypeError ()
expect t1 t2
  | equal t1 t2 = return ()
  | otherwise   = Left $ TypeMismatch t1 t2

typeOf :: Term -> Either TypeError Type
typeOf (Int _) = return IntType
typeOf (Add t1 t2) = do
  ty1 <- typeOf t1 >>= expect IntType
  ty2 <- typeOf t2 >>= expect IntType
  return IntType
