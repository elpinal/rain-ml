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

  describe "fromList" $
    it "constructs an ordered map from a list" $ do
      fromList []                `shouldBe` return (empty :: OrderedMap Int String)
      fromList [(True, "hello")] `shouldBe` insert True "hello" empty

      fromList [(True, "hello"), (False, " world")] `shouldBe` (insert True "hello" empty >>= insert False " world")

      fold <$> (fromList [('c', "hello"), ('b', " world"), ('a', "!")]) `shouldBe` return "hello world!"
      fold <$> (fromList [('0', "hello"), ('Z', " world"), ('r', "!")]) `shouldBe` return "hello world!"
