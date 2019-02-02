module Data.OrderedMapSpec where

import Test.Hspec

import Data.Foldable
import Data.OrderedMap

spec :: Spec
spec = do
  describe "OrderedMap" $
    it "is an instance of Foldable" $ do
      fold (empty :: OrderedMap Int String)                       `shouldBe` ""
      fold <$> insert True "hi" empty                             `shouldBe` Just "hi"
      fold <$> (insert True "hi" empty >>= insert False " there") `shouldBe` Just "hi there"
      fold <$> (insert False " there" empty >>= insert True "hi") `shouldBe` Just " therehi"

      fold <$> (insert False " there" empty >>= insert False "hi") `shouldBe` Nothing

      fold <$> (insert 'b' "hi" empty >>= insert 'a' " there" >>= insert 'c' "!") `shouldBe` Just "hi there!"
      fold <$> (insert 'c' "hi" empty >>= insert 'b' " there" >>= insert 'a' "!") `shouldBe` Just "hi there!"
