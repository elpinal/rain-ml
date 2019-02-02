{-# LANGUAGE BinaryLiterals #-}

module Language.RainML.CodeGenSpec where

import Test.Hspec

import qualified Data.ByteString.Lazy as B

import Data.OrderedMap

import Language.RainML.Asm
import Language.RainML.CodeGen

spec :: Spec
spec = do
  describe "codeGen" $
    it "generates code" $ do
      let gen = B.unpack . codeGen

      gen (Program (Block []) empty) `shouldBe` [1, 0b1000]

      gen <$> (Program (Block []) <$> (return empty >>= insert (Label "a") (IntType, Block [])))
        `shouldBe` Just [1, 0b1000, 0b1000]
