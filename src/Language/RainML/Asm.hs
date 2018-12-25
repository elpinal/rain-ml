{-# LANGUAGE ViewPatterns #-}

module Language.RainML.Asm
  ( makeGraph
  , Graph
  , Value(..)
  , Operand(..)
  , Inst(..)
  , Block(..)
  , Reg(..)
  ) where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Data.Coerce
import Data.Heap (Heap)
import qualified Data.Heap as Heap
import Data.List
import qualified Data.Map.Lazy as Map
import qualified Data.Set as Set
import Data.Word

import qualified Language.RainML.Intermediate as I

newtype Reg = Reg Word8
  deriving (Eq, Ord, Show)

data Value
  = Imm Word32
  deriving (Eq, Show)

data Operand
  = Register Reg
  | Value Value
  deriving (Eq, Show)

data Inst
  = Mov Reg Operand
  | Add Reg Reg Operand
  deriving (Eq, Show)

data Block = Block [Inst]
  deriving (Eq, Show)

type LiveVars = Set.Set Int
type Graph = Map.Map Int (Set.Set Int)

type Counter = State Int

inc :: StateT LiveVars Counter ()
inc = lift $ modify (+ 1)

liveVars :: I.Value -> LiveVars
liveVars (I.Var n) = Set.singleton n
liveVars _         = mempty

interfere :: Graph -> Int -> LiveVars -> Graph
interfere graph n ls
  | Set.null ls = graph
  | otherwise   = g $ Map.alter f n graph
  where
    f :: Maybe (Set.Set Int) -> Maybe (Set.Set Int)
    f Nothing   = return ls
    f (Just ks) = return $ ls <> ks

    g :: Graph -> Graph
    g graph1 = Map.unionWith (<>) (Map.fromSet (const $ Set.singleton n) ls) graph1

makeGraph :: I.Term -> Graph
makeGraph t = evalState (evalStateT (buildGraph t) mempty) 0

buildGraph :: I.Term -> StateT LiveVars Counter Graph
buildGraph (I.Value v) = do
  modify $ (<>) $ liveVars v
  return mempty -- Assumes values does not include two or more variables.
buildGraph (I.Let d t) = do
  graph <- buildGraph t
  buildGraphDecl graph d

buildGraphDecl :: Graph -> I.Decl -> StateT LiveVars Counter Graph
buildGraphDecl graph d = inc >> buildGraphDecl_ graph d

buildGraphDecl_ :: Graph -> I.Decl -> StateT LiveVars Counter Graph
buildGraphDecl_ graph (I.Id v) = do
  i <- lift get
  ls <- gets $ Set.delete $ i - 1
  let newLs = Set.map (+ i) $ liveVars v
  put $ newLs <> ls
  return $ interfere graph (i - 1) (ls Set.\\ newLs)

buildGraphDecl_ graph (I.Arith _ n v2) = do
  i <- lift get
  ls <- gets $ Set.delete $ i - 1
  let newLs = Set.map (+ i) $ liveVars (I.Var n) <> liveVars v2
  put $ newLs <> ls
  return $ interfere graph (i - 1) ls

newtype Weight = Weight Int
  deriving (Eq, Show)

instance Ord Weight where
  (Weight x) <= (Weight y) = x >= y

incWeight :: Weight -> Weight
incWeight (Weight n) = Weight $ n + 1

neighbors :: Int -> Graph -> Set.Set Int
neighbors n graph = Map.findWithDefault mempty n graph

-- Maximum cardinality search.
mcs :: Heap (Heap.Entry Weight Int) -> Graph -> [Int]
mcs (Heap.viewMin -> Just (Heap.payload -> n, heap)) graph = n : mcs (Heap.map f heap) (Map.delete n graph)
  where
    f e @ (Heap.Entry w m)
      | m `Set.member` (Map.keysSet graph `Set.intersection` neighbors n graph) = Heap.Entry (incWeight w) m
      | otherwise = e
mcs _ _ = []

newtype Color = Color Int
  deriving (Eq, Show)

color :: [Int] -> Graph -> Map.Map Int Color
color ns graph = foldl (\m n -> Map.insert n (colorEach n graph m) m) mempty ns

minfree :: [Color] -> Color
minfree xs = coerce $ minfrom 0 (length xs, coerce xs)

minfrom :: Int -> (Int, [Int]) -> Int
minfrom a (n, xs)
  | n == 0     = a
  | m == b - a = minfrom b (n - m, vs)
  | otherwise  = minfrom a (m, us)
    where
      (us, vs) = partition (< b) xs
      b = a + 1 + n `div` 2
      m = length us

colorEach :: Int -> Graph -> Map.Map Int Color -> Color
colorEach n graph m = minfree $ Map.elems $ Map.restrictKeys m $ neighbors n graph Set.\\ Map.keysSet m
