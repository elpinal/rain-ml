module Language.RainML.CodeGen
  ( codeGen
  ) where

import qualified Data.ByteString as B

import Language.RainML.Version

codeGen :: Int -> B.ByteString
codeGen n = B.pack [rainvmVersion, fromIntegral n]
