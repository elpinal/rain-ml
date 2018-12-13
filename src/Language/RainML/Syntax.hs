module Language.RainML.Syntax
  ( Term(..)
  , Literal(..)
  , Type(..)
  , TypeError(..)
  , Typing(..)
  , expect

  , Position(..)
  , SourcePos(..)
  ) where

import Text.Megaparsec.Pos

data Position = Position
  { start :: SourcePos
  , end   :: SourcePos
  }
  deriving (Eq, Show)

data Type
  = IntType
  | BoolType
  deriving (Eq, Show)

data Literal
  = Int Position Int
  | Bool Position Bool
  deriving (Eq, Show)

data Term
  = Add Position Term Term
  | Lit Position Literal
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
  typeOf (Int _ _)  = return IntType
  typeOf (Bool _ _) = return BoolType

instance Typing Term where
  typeOf (Lit _ l) = typeOf l
  typeOf (Add _ t1 t2) = do
    typeOf t1 >>= expect IntType
    typeOf t2 >>= expect IntType
    return IntType
