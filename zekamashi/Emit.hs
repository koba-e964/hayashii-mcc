module Emit where

import Data.String
import qualified Data.List as List

import Syntax
import Id
import Type
import Inst
import SSA



emitFundef :: SSAFundef -> [ZekInst]
emitFundef (SSAFundef nty params formFV blks) =
  concatMap emitBlock blks

emitBlock :: Block -> [ZekInst]
emitBlock (Block blkId insts term) = concatMap emitInst insts ++ emitTerm term

emitInst (Inst dest op) = emitSub dest op
  

emitTerm (TRet v) = emitSub (Just (VId "$0")) (SId v)


emitSub :: Maybe VId -> Op -> [ZekInst]
emitSub Nothing (SCall (LId lid :-: ty) ops) = [Bsr (Reg 28) lid]
emitSub Nothing _ = []
emitSub (Just (VId nm)) (SCall (LId lid :-: ty) ops) = [Bsr (Reg 28) lid, cp "$0" nm]
emitSub (Just (VId nm)) (SId (OpVar (VId src :-: ty))) = [Lda (regOfString nm) 0 (regOfString src)]
emitSub (Just (VId nm)) (SId (OpConst (IntConst x))) = [Lda (Reg 0) (fromIntegral x) (Reg 31)]
emitSub (Just (VId nm)) (SArithBin Add (OpVar (VId src :-: ty)) op2) = [Addl (regOfString src) (regimmOfOperand op2) (regOfString nm)]


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
