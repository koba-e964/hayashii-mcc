module Emit where

import Data.String
import qualified Data.List as List
import Control.Monad.State (State, gets, modify, evalState)

import Syntax
import Id
import Type
import Inst
import SSA hiding (M)

import qualified Data.Map as Map

data Env = Env { labelMap :: !(Map.Map BlockID Label), blkIdx :: !Int, currentFunction :: !String } 

type M = State Env
runM :: M a -> a
runM x = evalState x (Env Map.empty 0 "undefined><><")

data TailInfo = Tail | NonTail !(Maybe VId)

emit :: [SSAFundef] -> [ZekInst]
emit fundefs = runM $ do
  sub <- fmap concat $ mapM emitFundef fundefs
  return $ [Br rtmp "main"] ++ sub


emitFundef :: SSAFundef -> M [ZekInst]
emitFundef (SSAFundef (LId nm :-: ty) params formFV blks) = do
  modify $ \s -> s { currentFunction = nm }
  entryLabel <- freshLabel "entry"
  res <- fmap concat $ mapM emitBlock blks
  return $ [Label nm, Br rtmp entryLabel] ++ res

emitBlock :: Block -> M [ZekInst]
emitBlock (Block blkId phi insts term) = do
  lbl <- freshLabel blkId
  ii <- fmap concat $ mapM emitInst insts
  ti <- emitTerm term
  return $ [Label lbl] ++ ii ++ ti
emitInst (Inst dest op) = emitSub (NonTail dest) op

emitTerm :: Term -> M [ZekInst]
emitTerm (TRet (OpConst UnitConst)) =
  return [Ret rtmp rlr]
emitTerm (TRet v) = do
  sub <- emitSub Tail (SId v)
  return $ sub ++ [Ret rtmp rlr]
emitTerm (TJmp blk) = return [Br rtmp blk]
emitTerm (TBr (OpVar (VId src :-: ty)) blk1 blk2)
  = do
  l1 <- freshLabel blk1
  l2 <- freshLabel blk2
  return [ BC NE (regOfString src) l1
    , Br rtmp l2
    ]

emitSub :: TailInfo -> Op -> M [ZekInst]
emitSub Tail (SCall (LId lid :-: ty) ops) = return [Br rlr lid]
emitSub (NonTail Nothing) (SCall (LId lid :-: ty) ops) = return [Bsr rlr lid]
emitSub (NonTail Nothing) _ = return [] -- if not SCall there is no side-effect.
emitSub (NonTail (Just (VId nm))) o@(SCall (LId lid :-: ty) ops)
  | typeOfOp o == TFloat =
  let q = emitArgs [] ops in
  return $ q ++ [Bsr rlr lid] ++ fmov (FReg 0) (fregOfString nm)
emitSub (NonTail (Just (VId nm))) (SCall (LId lid :-: ty) ops) =
  let q = emitArgs [] ops in
  return $ q ++ [Bsr rlr lid] ++ cp "$0" nm
emitSub Tail (SCall (LId lid :-: ty) ops) = return [Br rtmp lid]
emitSub (NonTail (Just (VId nm))) o@(SId (OpVar (VId src :-: ty)))
  | typeOfOp o == TFloat =
    return $ fmov (fregOfString src) (fregOfString nm)
emitSub (NonTail (Just (VId nm))) (SId (OpVar (VId src :-: ty))) =
    return $ cp src nm
emitSub (NonTail (Just (VId nm))) (SId (OpConst (IntConst x))) =
    return $ li32 (fromIntegral x) (regOfString nm)
emitSub (NonTail (Just (VId nm))) (SArithBin aop (OpVar (VId src :-: ty)) op2) =
  let ctor = case aop of
        Add -> Addl
        Sub -> Subl
        Mul -> undefined
        Div -> undefined
  in return $ [ctor (regOfString src) (regimmOfOperand op2) (regOfString nm)]
emitSub (NonTail (Just (VId nm))) (SFloatBin bop (OpVar (VId src1 :-: _)) (OpVar (VId src2 :-: _))) =
  let ctor = case bop of
        FAdd -> FOpAdd
        FSub -> FOpSub
        FMul -> FOpMul
        FDiv -> undefined
  in
    return $ [FOp ctor (fregOfString src1) (fregOfString src2) (fregOfString nm)]
emitSub (NonTail (Just (VId nm))) (SCmpBin cop o@(OpVar (VId src1 :-: _)) (OpVar (VId src2 :-: _)))
  | getType o == TFloat =
  let ctor = case cop of
        Syntax.Eq -> CEQ
        Syntax.LE -> CLE
  in
    return $ [Cmps ctor (fregOfString src1) (fregOfString src2) (fregOfString nm)]
emitSub (NonTail (Just (VId nm))) (SCmpBin cop (OpVar (VId src :-: ty)) op2) =
  let ctor = case cop of
        Syntax.Eq -> CEQ
        Syntax.LE -> CLE
  in
    return $ [Inst.Cmp ctor (regOfString src) (regimmOfOperand op2) (regOfString nm)]
emitSub (NonTail (Just (VId nm))) (SCmpBin Syntax.Eq op1 (OpVar (VId src :-: ty))) =
  return $ [Inst.Cmp CEQ (regOfString src) (regimmOfOperand op1) (regOfString nm)]
emitSub (NonTail (Just (VId nm))) (SCmpBin Syntax.LE op1 (OpVar (VId src :-: ty))) =
  let retReg = (regOfString nm) in
  return $ [ Inst.Cmp CLT (regOfString src) (regimmOfOperand op1) retReg
  ] ++ abstAdd retReg (-1) retReg
  
emitSub Tail exp@(SId op) = emitSub (NonTail (Just (retReg (getType op)))) exp
emitSub Tail exp@(SArithBin {}) = emitSub (NonTail (Just (retReg TInt))) exp
emitSub Tail exp@(SCmpBin {}) = emitSub (NonTail (Just (retReg TInt))) exp
emitSub Tail exp@(SFNeg {}) = emitSub (NonTail (Just (retReg TFloat))) exp
emitSub Tail exp@(SFloatBin {}) = emitSub (NonTail (Just (retReg TFloat))) exp
emitSub Tail exp@(SNeg {}) = emitSub (NonTail (Just (retReg TInt))) exp
emitSub x y = error $ "undefined behavior in emitSub: " ++ show y 

retReg :: Type -> VId
retReg ty =
  case ty of
    TFloat -> VId "$f0"
    _      -> VId "$0"

cp :: String -> String -> [ZekInst]
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
    movOperand (OpVar (a :-: _)) (OpVar (b :-: _)) = mov (regOfString a) (regOfString b)
    movOperand (OpConst (IntConst v)) (OpVar (b :-: _)) = li32 (fromIntegral v) (regOfString b)
    fmovOperand (OpVar (a :-: _)) (OpVar (b :-: _)) = fmov (fregOfString a) (fregOfString b)
    fmovOperand (OpConst (FloatConst v)) (OpVar (b :-: _)) = lfi (realToFrac v) (fregOfString b)
-- helper functions

-- GPR: $0 ~ $31 ($31 = 0)
-- Float: $f0 ~ $f31

{- 関数呼び出しのために引数を並べ替える (register shuffling) -}
shuffle :: Operand -> [(Operand, Operand)] -> [(Operand, Operand)]
shuffle sw xys =
  let (xys0, imm) = List.partition (\ (x, y) -> case y of { OpVar {} -> True; _ -> False; }) xys in
  {- remove identical moves -}
  let xys1 = List.filter (\ (x, y) -> x /= y) xys0 in
  {- find acyclic moves -}
  let sub1 = case List.partition (\ (_, y) -> List.lookup y xys1 /= Nothing) xys1 of
            ([], []) -> []
            ((x, y) : xys, []) -> {- no acyclic moves; resolve a cyclic move -}
                (y, sw) : (x, y) :
                  shuffle sw (List.map (\e -> case e of
                                    (y', z) | y == y' -> (sw, z)
                                    yz -> yz) xys)
            (xys, acyc) -> acyc ++ shuffle sw xys
  in sub1 ++ imm


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

freshLabel :: BlockID -> M Label
freshLabel blkId = do
  f <- gets currentFunction
  let uid = f ++ "." ++ blkId
  lm <- gets labelMap
  if Map.member uid lm then
    return $ lm Map.! uid
  else do
    x <- gets blkIdx
    let str = uid ++ "." ++ show x 
    modify $ \s -> s { labelMap = Map.insert uid str lm, blkIdx = x + 1 }
    return str

