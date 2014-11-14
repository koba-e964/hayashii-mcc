{-# LANGUAGE OverloadedStrings #-}

module SSA where


import Closure
import Type
import Id
import Syntax


import Control.Applicative ((<$>))
import Data.Int

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
  deriving (Eq, Show)

type BlockID = Int

data Inst = Inst !(Maybe VId) !Op
  deriving (Eq)

instance Show Inst where
  show (Inst Nothing op) = show op
  show (Inst (Just (VId v)) op) = "$" ++ v ++ " := " ++ show op


data Op = 
  SId !Operand
  | SArithBin !ArithBinOp !Operand
  | SFloatBin !FloatBinOp !Operand
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
  SSAFundef (LId lid :-: ty) (map (\(x,t) -> x :-: t) args) (map (\(x,t) -> x :-: t) formFV) <$> (fmap accBlocks $ execStateT (ssaTransExpr expr) emptyState)


ssaTransExpr :: ClosExp -> StateT CgenState M ()

ssaTransExpr (expr :-: ty) = case expr of
  CUnit -> addTerm (TRet (OpConst UnitConst))
  CInt v -> addTerm (TRet (OpConst (IntConst (fromIntegral v))))
  CFloat v -> addTerm (TRet (OpConst (FloatConst (fromRational (toRational v)))))
  CNeg vid  -> do
    fresh <- freshVar
    addInst (Inst (Just fresh) (SNeg (OpVar (vid :-: TInt))))
    addTerm (TRet (OpVar (fresh :-: TInt)))
data CgenState = CgenState
  { blockIdx :: Int
  , current :: [Inst]
  , accBlocks :: [Block]
  } deriving (Show)

emptyState :: CgenState
emptyState = CgenState 0 [] []

addInst :: Inst -> StateT CgenState M ()
addInst inst = do
  bid <- gets blockIdx
  is <- gets current
  modify (\s -> s { current = is ++ [inst] })


addTerm :: Term -> StateT CgenState M ()
addTerm t = do
  bid <- gets blockIdx
  insts <- gets current
  modify (\s -> s { accBlocks = accBlocks s ++ [Block bid insts t], blockIdx = bid + 1, current = [] })

freshVar :: StateT CgenState M VId
freshVar = do
  Id str <- lift $ genId "SSA"
  return (VId str)
