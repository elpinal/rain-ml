module Language.RainML.Syntax
  ( Term(..)
  , Literal(..)
  , Type(..)
  , TypeError(..)
  , Typing(..)
  , expect
  ) where

data Type
  = IntType
  | BoolType
  deriving (Eq, Show)

data Literal
  = Int Int
  | Bool Bool
  deriving (Eq, Show)

data Term
  = Add Term Term
  | Lit Literal
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

class Typing a where
  typeOf :: a -> Either TypeError Type

instance Typing Literal where
  typeOf (Int _)  = return IntType
  typeOf (Bool _) = return BoolType

instance Typing Term where
  typeOf (Lit l) = typeOf l
  typeOf (Add t1 t2) = do
    ty1 <- typeOf t1 >>= expect IntType
    ty2 <- typeOf t2 >>= expect IntType
    return IntType
