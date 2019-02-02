{-# LANGUAGE BinaryLiterals #-}

module Language.RainML.CodeGen
  ( codeGen
  ) where

import Data.Bits
import qualified Data.ByteString.Lazy as B
import Data.ByteString.Builder
import Data.Coerce
import Data.Word

import qualified Language.RainML.Asm as Asm
import Language.RainML.Version

data Instruction
  = Move
  | Halt
  | Add
  deriving (Eq, Show, Enum)

-- The second argument is given to provide additional information.
inst :: Instruction -> Word8 -> Builder
inst i extra = word8 $ shiftL (toEnum $ fromEnum i) 3 .|. extra

instWithoutInfo :: Instruction -> Builder
instWithoutInfo i = inst i 0

-- Bits to specify immediate versions of instructions.
immBits :: Word8
immBits = 0b100

codeGen :: Asm.Program -> B.ByteString
codeGen (Asm.Program entry _) = toLazyByteString $ mconcat
  [ word8 rainvmVersion
  , block entry
  ]

instruction :: Asm.Inst -> Builder
instruction (Asm.Mov r o)     = move r o
instruction (Asm.Add dr sr o) = add dr sr o

block :: Asm.Block -> Builder
block (Asm.Block is) = foldMap instruction is <> halt

toWord32 :: Asm.Value -> Word32
toWord32 (Asm.Imm w) = w

move :: Asm.Reg -> Asm.Operand -> Builder
move dest (Asm.Value v) = mconcat
  [ inst Move immBits
  , reg dest
  , word32BE $ toWord32 v
  ]
move dest (Asm.Register src) = mconcat
  [ inst Move $ shiftR (coerce src) 3
  , word8 $ coerce dest .|. shiftL (coerce src) 5
  ]

add :: Asm.Reg -> Asm.Reg -> Asm.Operand -> Builder
add dest src (Asm.Value v) = mconcat
  [ inst Add $ immBits .|. shiftR (coerce src) 3
  , word8 $ coerce dest .|. shiftL (coerce src) 5
  , word32BE $ toWord32 v
  ]
add dest src1 (Asm.Register src2) = mconcat
  [ inst Add $ shiftR (coerce src1) 3
  , word8 $ coerce src2 .|. shiftL (coerce src1) 5
  , word8 $ shiftL (coerce dest) 3
  ]

reg :: Asm.Reg -> Builder
reg = word8 . coerce

halt :: Builder
halt = instWithoutInfo Halt
