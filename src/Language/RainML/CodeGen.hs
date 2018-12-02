{-# LANGUAGE BinaryLiterals #-}

module Language.RainML.CodeGen
  ( codeGen
  ) where

import qualified Data.ByteString.Lazy as B
import Data.ByteString.Builder
import Data.Coerce
import Data.Word

import Language.RainML.Version

newtype Reg = Reg Word8
  deriving (Eq, Show)

codeGen :: Int -> B.ByteString
codeGen n = toLazyByteString $ mconcat
  [ word8 rainvmVersion
  , word8 0b100
  , reg $ Reg 0
  , word32BE $ fromIntegral n
  ]

reg :: Reg -> Builder
reg = word8 . coerce
