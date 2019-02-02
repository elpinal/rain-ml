{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Language.RainML.Asm
  ( Value(..)
  , Operand(..)
  , Inst(..)
  , Block(..)
  , Reg(..)

  , typecheckWhole
  , typecheck
  , Type(..)
  , Context(..)
  , TypeError(..)

  , makeGraph
  , IGraph

  , toAsm
  , TranslateError
  ) where

import Algebra.Graph.Class
import Algebra.Graph.Relation.Symmetric

import Control.Monad
import Control.Monad.Freer
import Control.Monad.Freer.Error
import Control.Monad.Freer.Reader
import qualified Control.Monad.Freer.State as FState
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Strict
import Data.Coerce
import Data.Foldable
import Data.Heap (Heap)
import qualified Data.Heap as Heap
import Data.List
import qualified Data.Map.Lazy as Map
import qualified Data.Set as Set
import Data.Word

import Data.OrderedMap (OrderedMap)
import qualified Data.OrderedMap as OMap

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

newtype Label = Label String
  deriving (Eq, Show)

data Program = Program Block (OrderedMap Label (Type, Block))
  deriving (Eq, Show)

data Type
  = IntType
  | Code Context
  deriving (Eq, Show)

newtype Context = Context (Map.Map Reg Type)
  deriving (Eq, Show)

emptyContext :: Context
emptyContext = Context Map.empty

mapContext :: (Map.Map Reg Type -> Map.Map Reg Type) -> Context -> Context
mapContext = coerce

newtype HeapContext = HeapContext (Map.Map Label Type)
  deriving (Eq, Show)

emptyHeapContext :: HeapContext
emptyHeapContext = HeapContext Map.empty

data TypeError
  = UnboundRegister Reg
  | NotIntType Type
  | NotContext Type
  deriving (Eq, Show)

type Effs = '[Reader HeapContext, Error TypeError]

class Typed a where
  type Output a

  -- | Additional effects.
  type AEffs a :: [* -> *]

  typeOf :: (Members (AEffs a) xs, Members Effs xs) => a -> Eff xs (Output a)

instance Typed Value where
  type Output Value = Type
  type AEffs Value = '[FState.State Context]

  typeOf (Imm _) = return IntType

lookupReg :: Members '[FState.State Context, Error TypeError] xs => Reg -> Eff xs Type
lookupReg r = do
  Context ctx <- FState.get
  case Map.lookup r ctx of
    Nothing -> throwError $ UnboundRegister r
    Just ty -> return ty

instance Typed Reg where
  type Output Reg = Type
  type AEffs Reg = '[FState.State Context]

  typeOf = lookupReg

instance Typed Operand where
  type Output Operand = Type
  type AEffs Operand = '[FState.State Context]

  typeOf (Register r) = typeOf r
  typeOf (Value v)    = typeOf v

updateReg :: Member (FState.State Context) xs => Reg -> Type -> Eff xs ()
updateReg r ty = FState.modify $ mapContext $ Map.insert r ty

expect :: (Typed a, Members (AEffs a) xs, Members Effs xs) => a -> (Output a -> Eff xs ()) -> Eff xs ()
expect x f = typeOf x >>= f

int :: Member (Error TypeError) xs => Type -> Eff xs ()
int IntType = return ()
int ty      = throwError $ NotIntType ty

instance Typed Inst where
  type Output Inst = ()
  type AEffs Inst = '[FState.State Context]

  typeOf (Mov r op)      = typeOf op >>= updateReg r
  typeOf (Add r op1 op2) = do
    expect op1 int
    expect op2 int
    updateReg r IntType

instance Typed Block where
  type Output Block = ()
  type AEffs Block = '[FState.State Context]

  typeOf (Block is) = mapM_ typeOf is

fromContext :: Member (Error TypeError) xs => Type -> Eff xs Context
fromContext (Code ctx) = return ctx
fromContext ty         = throwError $ NotContext ty

instance Typed Program where
  type Output Program = ()
  type AEffs Program = '[]

  typeOf (Program entry m) = do
    FState.evalState emptyContext $ typeOf entry
    traverse_ f $ OMap.toUnorderedMap m
      where
        f (ty, block) = do
          ctx <- fromContext ty
          FState.evalState ctx $ typeOf block

typecheck :: Context -> Block -> Either TypeError Context
typecheck ctx block = run $ runError $ FState.execState ctx $ runReader emptyHeapContext $ typeOf block

typecheckWhole :: Block -> Either TypeError ()
typecheckWhole = void . typecheck (Context mempty)

fromProgram :: Program -> HeapContext
fromProgram (Program _ m) = HeapContext $ OMap.toUnorderedMap $ fst <$> m

type LiveVars = Set.Set Int

-- | Represents an interference graph.
type IGraph = SymmetricRelation Int

type Counter = State Int

inc :: Monad m => StateT a (StateT Int m) ()
inc = lift $ modify (+ 1)

liveVars :: I.Value -> LiveVars
liveVars (I.Var n) = Set.singleton n
liveVars _         = mempty

interfere :: IGraph -> Int -> LiveVars -> IGraph
interfere graph n ls
  | Set.null ls = graph
  | otherwise   = graph + vertex n * vertices (Set.toList ls)

makeGraph :: I.Term -> IGraph
makeGraph t = evalState (evalStateT (buildGraph t) mempty) 0

buildGraph :: I.Term -> StateT LiveVars Counter IGraph
buildGraph (I.Value v) = do
  modify $ (<>) $ liveVars v
  return empty -- Assumes values does not include two or more variables.
buildGraph (I.Let d t) = do
  graph <- buildGraph t
  buildGraphDecl graph d

buildGraphDecl :: IGraph -> I.Decl -> StateT LiveVars Counter IGraph
buildGraphDecl graph d = inc >> buildGraphDecl_ graph d

buildGraphDecl_ :: IGraph -> I.Decl -> StateT LiveVars Counter IGraph
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

-- Maximum cardinality search.
mcs :: Heap (Heap.Entry Weight Int) -> IGraph -> [Int]
mcs (Heap.viewMin -> Just (Heap.payload -> n, heap)) graph = n : mcs (Heap.map f heap) graph
  where
    f e@(Heap.Entry w m)
      | m `Set.member` neighbours n graph = Heap.Entry (incWeight w) m
      | otherwise = e
mcs _ _ = []

newtype Color = Color { getColor :: Int }
  deriving (Eq, Show)

color :: [Int] -> IGraph -> Map.Map Int Color
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

colorEach :: Int -> IGraph -> Map.Map Int Color -> Color
colorEach n graph m = minfree $ Map.elems $ Map.restrictKeys m $ neighbours n graph

regalloc :: I.Term -> Map.Map Int Color
regalloc t = color ns graph
  where
    (graph, n) = runState (evalStateT (buildGraph t) mempty) 0
    ns = mcs (Heap.fromList $ map (Heap.Entry $ Weight 0) [0 .. n - 1]) graph

toAsm :: I.Term -> Either TranslateError Block
toAsm t = runExcept $ evalStateT (evalStateT (fromTerm t) $ regalloc t) 0

type Coloring = Map.Map Int Color
type Translator = StateT Coloring (StateT Int (Except TranslateError))

data TranslateError
  = RegisterSpilling
  deriving (Eq, Show)

fromTerm :: I.Term -> Translator Block
fromTerm (I.Value v) = fromValue v >>= \o -> return $ Block [Mov (Reg 0) o]
fromTerm (I.Let d t) = do
  Block is <- fromTerm t
  inc
  i <- fromDecl d
  return $ Block $ i : is

fromDecl :: I.Decl -> Translator Inst
fromDecl (I.Id v) = do
  o <- fromValue v
  r <- fromVar (-1)
  return $ Mov r o
fromDecl (I.Arith I.Add n v) = do
  o <- fromValue v
  s <- fromVar n
  d <- fromVar (-1)
  return $ Add d s o

fromValue :: I.Value -> Translator Operand
fromValue (I.Lit l) = fromLiteral l
fromValue (I.Var n) = Register <$> fromVar n

fromVar :: Int -> Translator Reg
fromVar n = do
  m <- get
  i <- lift get
  let c = Map.findWithDefault (error "fromVar: unexpected error") (n + i) m
  if getColor c < 16
    then return $ Reg $ fromIntegral $ getColor c
    else lift $ lift $ throwE RegisterSpilling

fromLiteral :: I.Literal -> Translator Operand
fromLiteral (I.Int n) = return $ Value $ Imm $ fromIntegral n
