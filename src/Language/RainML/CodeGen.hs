{-# LANGUAGE BinaryLiterals #-}

module Language.RainML.CodeGen
  ( codeGen
  ) where

import Data.Bits
import qualified Data.ByteString.Lazy as B
import Data.ByteString.Builder
import Data.Coerce
import Data.Word

import Language.RainML.Version

newtype Reg = Reg Word8
  deriving (Eq, Show)

newtype Opcode = Opcode Word8
  deriving (Eq, Show)

moveImm :: Word8
moveImm = 0b100

opcodeHalt :: Opcode
opcodeHalt = Opcode 1

codeGen :: Int -> B.ByteString
codeGen n = toLazyByteString $ mconcat
  [ word8 rainvmVersion
  , moveImmediate (Reg 0) n
  , halt
  ]

moveImmediate :: Reg -> Int -> Builder
moveImmediate dest imm = mconcat
  [ word8 moveImm
  , reg dest
  , word32BE $ fromIntegral imm
  ]

reg :: Reg -> Builder
reg = word8 . coerce

halt :: Builder
halt = opcode opcodeHalt

opcode :: Opcode -> Builder
opcode = word8 . (`shiftL` 3) . coerce
