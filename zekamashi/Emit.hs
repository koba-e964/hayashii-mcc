module Emit where

import Data.String
import qualified Data.List as List

import Syntax
import Id
import Type
import Inst
import SSA




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
emitSub (NonTail (Just (VId nm))) o@(SCall (LId lid :-: ty) ops)
  | typeOfOp o == TFloat =
  let q = emitArgs [] ops in
  q ++[Bsr rlr lid, fmov (FReg 0) (fregOfString nm)]
emitSub (NonTail (Just (VId nm))) (SCall (LId lid :-: ty) ops) =
  let q = emitArgs [] ops in
  q ++[Bsr rlr lid, cp "$0" nm]
emitSub Tail (SCall (LId lid :-: ty) ops) = [Br rtmp lid]
emitSub (NonTail (Just (VId nm))) o@(SId (OpVar (VId src :-: ty)))
  | typeOfOp o == TFloat =
    [fmov (fregOfString src) (fregOfString nm)]
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


emitArgs :: [(Reg, Reg)] -> [Operand] -> [ZekInst]
emitArgs x_reg_cl ops =
  let (ys, zs) = List.partition (\x -> getType x /= TFloat) ops in
  let (i, yrs) = List.foldl'
        (\(i, yrs) y -> (i + 1, (y, OpVar (VId (show (Reg i)) :-: getType y)) : yrs))
        (0, map (\(x, reg_cl) -> (operandOfReg x, operandOfReg reg_cl)) x_reg_cl) ys in
  let gprs = List.concatMap
        (\ (y, r) -> movOperand y r)
        (shuffle (operandOfReg rtmp) yrs) in
  let (d, zfrs) = List.foldl'
        (\(d, zfrs) z -> (d + 1, (z, OpVar (VId (show (FReg d)) :-: getType z)) : zfrs))
        (0, []) zs in
  let fregs = List.concatMap
        (\ (z, fr) -> fmovOperand z fr)
        (shuffle (OpVar (VId (show frtmp) :-: TFloat)) zfrs) in
    gprs ++ fregs
  where
    operandOfReg x = OpVar (VId (show x) :-: TInt)
    movOperand (OpVar (a :-: _)) (OpVar (b :-: _)) = [mov (regOfString a) (regOfString b)]
    movOperand (OpConst (IntConst v)) (OpVar (b :-: _)) = li32 (fromIntegral v) (regOfString b)
    fmovOperand (OpVar (a :-: _)) (OpVar (b :-: _)) = [fmov (fregOfString a) (fregOfString b)]
    fmovOperand (OpConst (FloatConst v)) (OpVar (b :-: _)) = lfi (realToFrac v) (fregOfString b)
-- helper functions

-- GPR: $0 ~ $31 ($31 = 0)
-- Float: $f0 ~ $f31

{- 関数呼び出しのために引数を並べ替える (register shuffling) -}
shuffle :: Eq a => a -> [(a, a)] -> [(a, a)]
shuffle sw xys =
  {- remove identical moves -}
  let xys1 = List.filter (\ (x, y) -> x /= y) xys in
    {- find acyclic moves -}
    case List.partition (\ (_, y) -> List.lookup y xys /= Nothing) xys of
      ([], []) -> []
      ((x, y) : xys, []) -> {- no acyclic moves; resolve a cyclic move -}
          (y, sw) : (x, y) :
            shuffle sw (List.map (\e -> case e of
                                    (y', z) | y == y' -> (sw, z)
                                    yz -> yz) xys)
      (xys, acyc) -> acyc ++ shuffle sw xys


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
