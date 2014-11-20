{-# LANGUAGE OverloadedStrings, FlexibleContexts, TypeOperators #-}
module SSAProp where

import Id
import SSA
import Type
import Control.Monad
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Typeable

type ConstEnv = Map VId Operand


propFundef :: SSAFundef -> SSAFundef
propFundef (SSAFundef nty args formFV blks) = 
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
    SId c -> do
      case dest of
        Nothing -> return ()
        Just dest' -> modify (Map.insert dest' c)
      return $ SId c
    SId o -> return $ SId $ prop env o
    SArithBin operator x y ->
      return $ SArithBin operator (prop env x) (prop env y)
    SNeg x ->
      return $ SNeg (prop env x)
    SFNeg x ->
      return $ SFNeg (prop env x)
    SFloatBin operator x y ->
      return $ SFloatBin operator (prop env x) (prop env y)
    SCall lid operands ->
      return $ SCall lid (map (prop env) operands)
  return $ Inst dest result


{- @prop env op@ returns op itself or constant assigned to @op@. -}
prop :: ConstEnv -> Operand -> Operand
prop env op = case op of
  OpVar (vid :-: _) | Map.member vid env -> env Map.! vid
  _ -> op

{- Performs constant propagation and copy propagation. This continues until no changes happen. -}
propagate :: [SSAFundef] -> [SSAFundef]
propagate = map f
  where
   f e = let e' = propFundef e in
    if e == e' then e else f e'

