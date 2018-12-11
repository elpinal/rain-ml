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

moveImm :: Word8
moveImm = 0b100

codeGen :: Int -> B.ByteString
codeGen n = toLazyByteString $ mconcat
  [ word8 rainvmVersion
  , moveImmediate (Reg 0) n
  ]

moveImmediate :: Reg -> Int -> Builder
moveImmediate dest imm = mconcat
  [ word8 moveImm
  , reg dest
  , word32BE $ fromIntegral imm
  ]

reg :: Reg -> Builder
reg = word8 . coerce
