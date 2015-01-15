module Emit where

import Data.String
import qualified Data.List as List

import Syntax
import Id
import Type
import Inst
import SSA

rtmp = Reg 28
rlr = Reg 29

emit :: [SSAFundef] -> [ZekInst]
emit fundefs = [Br rtmp "main"] ++ concatMap emitFundef fundefs


emitFundef :: SSAFundef -> [ZekInst]
emitFundef (SSAFundef (LId nm :-: ty) params formFV blks) =
  [Label nm] ++
   concatMap emitBlock blks
  ++ [Ret rtmp rlr]

emitBlock :: Block -> [ZekInst]
emitBlock (Block blkId insts term) = concatMap emitInst insts ++ emitTerm term

emitInst (Inst dest op) = emitSub True dest op
  

emitTerm (TRet v) = emitSub False (Just (VId "$0")) (SId v)


emitSub :: Bool -> Maybe VId -> Op -> [ZekInst]
emitSub _ Nothing (SCall (LId lid :-: ty) ops) = [Bsr (Reg 28) lid]
emitSub _ Nothing _ = []
emitSub True (Just (VId nm)) (SCall (LId lid :-: ty) ops) = [Bsr rlr lid, cp "$0" nm]
emitSub False (Just (VId nm)) (SCall (LId lid :-: ty) ops) = [Br rtmp lid]
emitSub _ (Just (VId nm)) (SId (OpVar (VId src :-: ty))) = [Lda (regOfString nm) 0 (regOfString src)]
emitSub _ (Just (VId nm)) (SId (OpConst (IntConst x))) = [Lda (Reg 0) (fromIntegral x) (Reg 31)]
emitSub _ (Just (VId nm)) (SArithBin Add (OpVar (VId src :-: ty)) op2) = [Addl (regOfString src) (regimmOfOperand op2) (regOfString nm)]


cp src dest = mov (regOfString src) (regOfString dest)

-- helper functions

-- GPR: $0 ~ $31 ($31 = 0)
-- Float: $f0 ~ $f31

regOfString :: (Eq s, IsString s, Show s) => s -> Reg
regOfString s = case List.elemIndex s [fromString $ "$" ++ show i | i <- [0..31]] of
  Just r  -> Reg r
  Nothing -> error $ "Invalid register name:" ++ show s

regimmOfOperand :: Operand -> RegImm
regimmOfOperand (OpConst (IntConst x)) = RIImm (fromIntegral x)
regimmOfOperand (OpVar (VId src :-: ty)) = case regOfString src of Reg x -> RIReg x
