module Language.RainML.AsmSpec where

import Test.Hspec

import Algebra.Graph.Class

import qualified Data.Map.Lazy as Map
import Data.Word

import Language.RainML.Asm hiding (Add, Value)
import qualified Language.RainML.Asm as Asm
import Language.RainML.Intermediate hiding (typecheck, IntType)

n5 :: Value
n5 = Lit $ Int 5

r4 :: Reg
r4 = Reg 4

r7 :: Reg
r7 = Reg 7

word :: Word32 -> Operand
word = Asm.Value . Imm

spec :: Spec
spec = do
  describe "makeGraph" $
    it "makes an interference graph from an intermediate term" $ do
      makeGraph (Value $ n5)                                                            `shouldBe` empty
      makeGraph (Value $ Var 0)                                                         `shouldBe` empty
      makeGraph (Let (Id n5) $ Value $ Var 0)                                           `shouldBe` empty
      makeGraph (Let (Id n5) $ Let (Id n5) $ Value $ Var 0)                             `shouldBe` empty
      makeGraph (Let (Id n5) $ Let (Id n5) $ Value $ Var 1)                             `shouldBe` 0 * 1
      makeGraph (Let (Id n5) $ Let (Id n5) $ Let (Arith Add 0 n5) $ Value $ Var 0)      `shouldBe` empty
      makeGraph (Let (Id n5) $ Let (Id n5) $ Let (Arith Add 0 (Var 1)) $ Value $ Var 0) `shouldBe` 1 * 2

      makeGraph (Let (Id n5) $ Let (Id n5) $ Let (Arith Add 0 (Var 1)) $ Let (Arith Add 0 (Var 2)) $ Value $ Var 0) `shouldBe` (1 + 2) * 3

  describe "typecheckBlock" $
    it "typechecks a program" $ do
      typecheckBlock (Context mempty) (Block []) `shouldBe` return (Context mempty)

      let ctx = Context $ Map.singleton r7 IntType in
        typecheckBlock ctx (Block []) `shouldBe` return ctx

      let ctx = Context mempty in do
        typecheckBlock ctx (Block [Mov r7 $ word 43])         `shouldBe` return (Context $ Map.singleton r7 IntType)
        typecheckBlock ctx (Block [Mov r7 $ Register r7])     `shouldBe` Left (UnboundRegister r7)
        typecheckBlock ctx (Block [Mov r4 $ Register r7])     `shouldBe` Left (UnboundRegister r7)
        typecheckBlock ctx (Block [Asm.Add r7 r4 $ word 100]) `shouldBe` Left (UnboundRegister r4)

      let ctx = Context $ Map.singleton r4 IntType in
        typecheckBlock ctx (Block [Asm.Add r7 r4 $ word 100]) `shouldBe` return (Context $ Map.fromList [(r4, IntType), (r7, IntType)])

      let ctx = Context $ Map.singleton r4 (Code $ Context mempty) in
        typecheckBlock ctx (Block [Asm.Add r7 r4 $ word 100]) `shouldBe` Left (NotIntType $ Code $ Context mempty)
