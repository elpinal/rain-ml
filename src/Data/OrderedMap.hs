{-# LANGUAGE DeriveFunctor #-}

module Data.OrderedMap
  ( OrderedMap
  , empty
  , insert

  -- * Conversion
  , toUnorderedMap
  ) where

import qualified Data.Map.Lazy as Map

data OrderedMap k a = OrderedMap [k] (Map.Map k a)
  deriving (Eq, Show, Functor)

empty :: OrderedMap k a
empty = OrderedMap [] Map.empty

insert :: Ord k => k -> a -> OrderedMap k a -> Maybe (OrderedMap k a)
insert k v (OrderedMap ks m)
  | Map.member k m = Nothing
  | otherwise      = Just $ OrderedMap (k : ks) $ Map.insert k v m

toUnorderedMap :: OrderedMap k a -> Map.Map k a
toUnorderedMap (OrderedMap _ m) = m

instance Ord k => Foldable (OrderedMap k) where
  foldr f e (OrderedMap ks m) = g ks e
    where
      g []       x = x
      g (l : ls) x = g ls $ f (Map.findWithDefault (error "foldr: unexpected error") l m) x
