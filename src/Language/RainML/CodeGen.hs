{-# LANGUAGE BinaryLiterals #-}

module Language.RainML.CodeGen
  ( codeGen
  ) where

import Data.Bits
import qualified Data.ByteString.Lazy as B
import Data.ByteString.Builder
import Data.Coerce
import Data.Word

import qualified Language.RainML.Syntax as S
import Language.RainML.Version

newtype Reg = Reg Word8
  deriving (Eq, Show)

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

codeGen :: S.Term -> B.ByteString
codeGen tm = toLazyByteString $ mconcat
  [ word8 rainvmVersion
  , term tm
  , halt
  ]

term :: S.Term -> Builder
term (S.Int n) = moveImmediate (Reg 0) n
term (S.Add (S.Int m) (S.Int n)) = moveImmediate (Reg 0) m <> addImmediate (Reg 0) (Reg 0) n
term (S.Add _ _) = error "not yet supported"

moveImmediate :: Reg -> Int -> Builder
moveImmediate dest imm = mconcat
  [ inst Move immBits
  , reg dest
  , word32BE $ fromIntegral imm
  ]

addImmediate :: Reg -> Reg -> Int -> Builder
addImmediate dest src imm = mconcat
  [ inst Add $ immBits .|. shiftR (coerce src) 3
  , word8 $ coerce dest .|. shiftL (coerce src) 5
  , word32BE $ fromIntegral imm
  ]

reg :: Reg -> Builder
reg = word8 . coerce

halt :: Builder
halt = instWithoutInfo Halt
