{-# LANGUAGE BinaryLiterals #-}

module Language.RainML.CodeGen
  ( codeGen
  ) where

import Data.Bits
import qualified Data.ByteString.Lazy as B
import Data.ByteString.Builder
import Data.Coerce
import Data.Word

import qualified Language.RainML.Intermediate as I
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

codeGen :: I.Term -> B.ByteString
codeGen tm = toLazyByteString $ mconcat
  [ word8 rainvmVersion
  , term tm
  , halt
  ]

value :: I.Value -> Builder
value (I.Int n) = moveImmediate (Reg 0) n
value (I.Var _) = mempty -- Subject to change.

decl :: I.Decl -> Builder
decl (I.Id (I.Int n))                    = moveImmediate (Reg 0) n
decl (I.Arith I.Add (I.Var _) (I.Int n)) = addImmediate (Reg 0) (Reg 0) n -- Subject to change.

term :: I.Term -> Builder
term (I.Value v) = value v
term (I.Let d t) = decl d <> term t

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
