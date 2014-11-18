{-# LANGUAGE OverloadedStrings, FlexibleContexts, TypeOperators #-}
module SSAOpt where

import Id
import SSA
import Type
import Control.Monad
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Typeable

type ConstEnv = Map VId Const


constProp :: SSAFundef -> SSAFundef
constProp (SSAFundef nty args formFV blks) = 
  (SSAFundef nty args formFV (map constPropBlock blks))

constPropBlock :: Block -> Block
constPropBlock blk = evalState (cpb blk) Map.empty

cpb :: (MonadState ConstEnv m) => Block -> m Block
cpb (Block blkId insts term) = do
  newInsts <- mapM propInst insts
  return $ Block blkId newInsts term

propInst :: (MonadState ConstEnv m) => Inst -> m Inst
propInst (Inst dest op) = do
  env <- get
  result <- case op of
    SId (OpConst c) -> do
      case dest of
        Nothing -> return ()
        Just dest' -> modify (Map.insert dest' c)
      return $ SId (OpConst c)
    SId o -> return $ SId $ prop env o
    SArithBin operator x y ->
      return $ SArithBin operator (prop env x) (prop env y)
    e -> return $ e
  return $ Inst dest result


prop :: ConstEnv -> Operand -> Operand
prop env op = case op of
  OpVar (vid :-: _) | Map.member vid env -> OpConst (env Map.! vid)
  _ -> op

