module Language.RainML.Asm
  ( makeGraph
  , Graph
  ) where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import qualified Data.Map.Lazy as Map
import qualified Data.Set as Set

import qualified Language.RainML.Intermediate as I

data Inst
  = Mov
  | Add
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

buildGraphDecl_ graph (I.Arith _ v1 v2) = do
  i <- lift get
  ls <- gets $ Set.delete $ i - 1
  let newLs = Set.map (+ i) $ liveVars v1 <> liveVars v2
  put $ newLs <> ls
  return $ interfere graph (i - 1) ls
