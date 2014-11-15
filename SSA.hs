{-# LANGUAGE OverloadedStrings #-}

module SSA where


import Closure
import Type
import Id
import Syntax


import Control.Applicative ((<$>))
import Data.Int
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe

import Control.Monad.Identity
import Control.Monad.State

type M = CounterT Identity


data SSAFundef = SSAFundef
  { name :: !(Typed LId)
  , args :: ![Typed VId]
  , formalFV :: ![Typed VId]
  , blocks :: ![Block]
  } deriving (Eq, Show)

data Block = Block !BlockID ![Inst] !Term
  deriving (Eq)

instance Show Block where
  show (Block bid insts term) = 
    bid ++ ":\n" ++
    concatMap (\x -> show x ++ "\n") insts ++
    show term ++ "\n"

type BlockID = String

data Inst = Inst !(Maybe VId) !Op
  deriving (Eq)

instance Show Inst where
  show (Inst Nothing op) = show op
  show (Inst (Just (VId v)) op) = "$" ++ v ++ " := " ++ show op


data Op = 
  SId !Operand
  | SArithBin !ArithBinOp !Operand !Operand
  | SFloatBin !FloatBinOp !Operand !Operand
  | SNeg !Operand
  | SFNeg !Operand
  | SCall !(Typed LId) ![Typed VId]
  deriving (Eq, Show)
data Term =
  TRet !Operand
  | TBr !Operand !BlockID !BlockID
  deriving (Eq, Show)
data Operand = OpVar !(Typed VId) | OpConst !Const
  deriving (Eq)

instance Show Operand where
  show (OpVar (VId v :-: _)) = "$" ++ v
  show (OpConst c) = show c


data Const = 
  IntConst !Int32
  | FloatConst !Double
  | UnitConst
  deriving (Eq)

instance Show Const where
  show (IntConst x) = show x
  show (FloatConst x) = show x
  show UnitConst = "()"

ssaTrans :: [CFundef] -> ClosExp -> M [SSAFundef]
ssaTrans defs (expr :-: ty) = do
  defsTrans <- mapM ssaTransFun defs
  mainTrans <- ssaTransFun (CFundef ("main", TFun [] ty) [] [] (expr :-: ty))
  return (defsTrans ++ [mainTrans])

ssaTransFun :: CFundef -> M SSAFundef
ssaTransFun (CFundef (VId lid, ty) args formFV expr) =
  SSAFundef (LId lid :-: ty) (map (\(x,t) -> x :-: t) args) (map (\(x,t) -> x :-: t) formFV) <$> (fmap getBlocks $ execStateT (ssaTransExpr expr) emptyState)


ssaTransExpr :: ClosExp -> StateT CgenState M ()
getOperand :: ClosExp -> StateT CgenState M Operand

ssaTransExpr exprty@(expr :-: ty) = do
  op <- getOperand exprty
  addTerm (TRet op)
getOperand (expr :-: ty) = case expr of
  CUnit -> return (OpConst UnitConst)
  CInt v -> return  (OpConst (IntConst (fromIntegral v)))
  CFloat v -> return (OpConst (FloatConst (fromRational (toRational v))))
  CNeg vid  -> do
    fresh <- freshVar
    addInst (Inst (Just fresh) (SNeg (OpVar (vid :-: TInt))))
    return (OpVar (fresh :-: TInt))
  CLet vid ty e1 e2 -> do
    res <- getOperand e1
    addInst (Inst (Just vid) (SId res))
    getOperand e2
  CVar vid -> return (OpVar (vid :-: ty))
  CArithBin op x y -> do
    fresh <- freshVar
    addInst $ Inst (Just fresh) $ SArithBin op (OpVar (x :-: TInt)) (OpVar (y :-: TInt))
    return (OpVar (fresh :-: TInt))
data CgenState = CgenState
  { blockIdx :: Int
  , current :: String
  , accBlocks :: Map String Block
  } deriving (Show)

emptyState :: CgenState
emptyState = CgenState 0 "entry" (Map.fromList [("entry", Block "entry" [] (TRet (OpConst UnitConst)))])

addInst :: Inst -> StateT CgenState M ()
addInst inst = do
  nm <- gets current
  Block blkId insts term <- gets (fromJust . Map.lookup nm . accBlocks)
  modify (\s -> s { accBlocks = Map.insert nm (Block blkId (insts ++ [inst]) term) (accBlocks s) })


addTerm :: Term -> StateT CgenState M ()
addTerm term = do
  nm <- gets current
  Block blkId insts _ <- gets (fromJust . Map.lookup nm . accBlocks)
  modify (\s -> s { accBlocks = Map.insert nm (Block blkId insts term) (accBlocks s) })

freshVar :: StateT CgenState M VId
freshVar = do
  Id str <- lift $ genId "SSA"
  return (VId str)

getBlocks :: CgenState -> [Block]
getBlocks st = map snd (Map.toList (accBlocks st))

