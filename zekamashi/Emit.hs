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


data TailInfo = Tail | NonTail !(Maybe VId)

emit :: [SSAFundef] -> [ZekInst]
emit fundefs = [Br rtmp "main"] ++ concatMap emitFundef fundefs


emitFundef :: SSAFundef -> [ZekInst]
emitFundef (SSAFundef (LId nm :-: ty) params formFV blks) =
  [Label nm] ++
   concatMap emitBlock blks
  ++ [Ret rtmp rlr]

emitBlock :: Block -> [ZekInst]
emitBlock (Block blkId insts term) = concatMap emitInst insts ++ emitTerm term

emitInst (Inst dest op) = emitSub (NonTail dest) op


emitTerm (TRet v) = emitSub Tail (SId v)


emitSub :: TailInfo -> Op -> [ZekInst]
emitSub Tail (SCall (LId lid :-: ty) ops) = [Br rlr lid]
emitSub (NonTail Nothing) (SCall (LId lid :-: ty) ops) = [Bsr rlr lid]
emitSub (NonTail Nothing) _ = [] -- if not SCall there is no side-effect.
emitSub (NonTail (Just (VId nm))) (SCall (LId lid :-: ty) ops) = [Bsr rlr lid, cp "$0" nm]
emitSub Tail (SCall (LId lid :-: ty) ops) = [Br rtmp lid]
emitSub (NonTail (Just (VId nm))) (SId (OpVar (VId src :-: ty))) =
    [Lda (regOfString nm) 0 (regOfString src)]
emitSub (NonTail (Just (VId nm))) (SId (OpConst (IntConst x))) =
    [Lda (regOfString nm) (fromIntegral x) (Reg 31)]
emitSub (NonTail (Just (VId nm))) (SArithBin aop (OpVar (VId src :-: ty)) op2) =
  let ctor = case aop of
        Add -> Addl
        Sub -> Subl
        Mul -> undefined
        Div -> undefined
  in [ctor (regOfString src) (regimmOfOperand op2) (regOfString nm)]
emitSub (NonTail (Just (VId nm))) (SFloatBin bop (OpVar (VId src1 :-: _)) (OpVar (VId src2 :-: _))) =
  let ctor = case bop of
        FAdd -> FOpAdd
        FSub -> FOpSub
        FMul -> FOpMul
        FDiv -> undefined
  in
    [FOp ctor (fregOfString src1) (fregOfString src2) (fregOfString nm)]
emitSub Tail exp@(SId op) = emitSub (NonTail (Just (retReg (getType op)))) exp
emitSub Tail exp@(SArithBin {}) = emitSub (NonTail (Just (retReg TInt))) exp
emitSub Tail exp@(SCmpBin {}) = emitSub (NonTail (Just (retReg TInt))) exp
emitSub Tail exp@(SFNeg {}) = emitSub (NonTail (Just (retReg TFloat))) exp
emitSub Tail exp@(SFloatBin {}) = emitSub (NonTail (Just (retReg TFloat))) exp
emitSub Tail exp@(SNeg {}) = emitSub (NonTail (Just (retReg TInt))) exp
emitSub Tail exp@(SPhi {}) = emitSub (NonTail (Just (retReg TInt))) exp

retReg :: Type -> VId
retReg ty =
  case ty of
    TFloat -> VId "$f0"
    _      -> VId "$0"

cp :: String -> String -> ZekInst
cp src dest = mov (regOfString src) (regOfString dest)

-- helper functions

-- GPR: $0 ~ $31 ($31 = 0)
-- Float: $f0 ~ $f31

regOfString :: (Eq s, IsString s, Show s) => s -> Reg
regOfString s = case List.elemIndex s [fromString $ "$" ++ show i | i <- [0..31 :: Int]] of
  Just r  -> Reg r
  Nothing -> error $ "Invalid register name:" ++ show s

fregOfString :: (Eq s, IsString s, Show s) => s -> FReg
fregOfString s = case List.elemIndex s [fromString $ "$f" ++ show i | i <- [0..31 :: Int]] of
  Just r  -> FReg r
  Nothing -> error $ "Invalid register name:" ++ show s

regimmOfOperand :: Operand -> RegImm
regimmOfOperand (OpConst (IntConst x)) = RIImm (fromIntegral x)
regimmOfOperand (OpVar (VId src :-: ty)) = case regOfString src of Reg x -> RIReg x
