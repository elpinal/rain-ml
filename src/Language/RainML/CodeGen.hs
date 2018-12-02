module Language.RainML.CodeGen
  ( codeGen
  ) where

import qualified Data.ByteString.Lazy as B
import Data.ByteString.Builder

import Language.RainML.Version

codeGen :: Int -> B.ByteString
codeGen n = toLazyByteString $ word8 rainvmVersion <> word32BE (fromIntegral n)
