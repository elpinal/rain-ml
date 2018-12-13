{-# LANGUAGE DeriveFunctor #-}

module Language.RainML.Syntax
  ( Term(..)
  , Literal(..)
  , Type(..)
  , TypeError(..)
  , Typing(..)
  , typecheck

  , Positional(..)
  , Position(..)
  , SourcePos(..)

  , Display(..)
  ) where

import Text.Megaparsec.Pos

data Position = Position
  { start :: SourcePos
  , end   :: SourcePos
  }
  deriving (Eq, Show)

class Display a where
  displays :: a -> ShowS
  display :: a -> String

  displays x s = display x ++ s
  display x = displays x ""

instance Display Position where
  displays p = showString (sourcePosPretty $ start p) . showString ".." . showEnd (end p)
    where
      showEnd s = showPos (sourceLine s) . showChar ':' . showPos (sourceColumn s)
      showPos = shows . unPos

data Positional a = Positional
  { getPosition :: Position
  , fromPositional :: a
  }
  deriving (Eq, Show, Functor)

instance Show a => Display (Positional a) where
  displays p = displays (getPosition p) . showString ": " . shows (fromPositional p)

data Type
  = IntType
  | BoolType
  deriving (Eq, Show)

instance Display Type where
  display IntType  = "int"
  display BoolType = "bool"

data Literal
  = Int Int
  | Bool Bool
  deriving (Eq, Show)

data Term
  = Add (Positional Term) (Positional Term)
  | Lit (Positional Literal)
  deriving (Eq, Show)

data TypeError
  = TypeMismatch Position Type Type
  deriving (Eq, Show)

instance Display TypeError where
  displays (TypeMismatch p t1 t2) = displays p . showString ": type mismatch: expected `" . displays t1 . showString "`, but found `" . displays t2 . showChar '`'

-- Type equality.
equal :: Type -> Type -> Bool
equal = (==)

expect :: Position -> Type -> Type -> Either TypeError ()
expect pos t1 t2
  | equal t1 t2 = return ()
  | otherwise   = Left $ TypeMismatch pos t1 t2

typecheck :: Positional Term -> Either TypeError ()
typecheck t = typeOf (fromPositional t) >>= expect (getPosition t) IntType

class Typing a where
  typeOf :: a -> Either TypeError Type

instance Typing Literal where
  typeOf (Int _)  = return IntType
  typeOf (Bool _) = return BoolType

instance Typing Term where
  typeOf (Lit l) = typeOf $ fromPositional l
  typeOf (Add t1 t2) = do
    typeOf (fromPositional t1) >>= expect (getPosition t1) IntType
    typeOf (fromPositional t2) >>= expect (getPosition t2) IntType
    return IntType
