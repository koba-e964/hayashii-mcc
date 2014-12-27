module Emit where

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
emitSub Nothing (SCall (lid :-: ty) ops) = undefined
emitSub Nothing _ = []
emitSub (Just (VId nm)) (SId (OpVar (VId src :-: ty))) = [Lda (Reg 0) 0 (Reg 0)]
emitSub (Just (VId nm)) (SId (OpConst (IntConst x))) = [Lda (Reg 0) (fromIntegral x) (Reg 31)]
