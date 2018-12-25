module Language.RainML.AsmSpec where

import Test.Hspec

import qualified Data.Map.Lazy as Map
import qualified Data.Set as Set

import Language.RainML.Asm hiding (Add, Value)
import Language.RainML.Intermediate

true :: Value
true = Lit $ Bool True

spec :: Spec
spec = do
  describe "makeGraph" $
    it "makes an interfere graph from an intermediate term" $ do
      makeGraph (Value $ true)                                                                    `shouldBe` mempty
      makeGraph (Value $ Var 0)                                                                   `shouldBe` mempty
      makeGraph (Let (Id true) $ Value $ Var 0)                                                   `shouldBe` mempty
      makeGraph (Let (Id true) $ Let (Id true) $ Value $ Var 0)                                   `shouldBe` mempty
      makeGraph (Let (Id true) $ Let (Id true) $ Value $ Var 1)                                   `shouldBe` Map.fromList [(0, Set.singleton 1), (1, Set.singleton 0)]
      makeGraph (Let (Id true) $ Let (Id true) $ Let (Arith Add true true) $ Value $ Var 0)       `shouldBe` mempty
      makeGraph (Let (Id true) $ Let (Id true) $ Let (Arith Add (Var 0) (Var 1)) $ Value $ Var 0) `shouldBe` Map.fromList [(1, Set.singleton 2), (2, Set.singleton 1)]

      makeGraph (Let (Id true) $ Let (Id true) $ Let (Arith Add (Var 0) (Var 1)) $ Let (Arith Add (Var 0) (Var 2)) $ Value $ Var 0)
        `shouldBe` Map.fromList [(1, Set.singleton 3), (2, Set.singleton 3), (3, Set.fromList [1, 2])]
