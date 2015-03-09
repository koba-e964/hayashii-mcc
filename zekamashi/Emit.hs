module Emit where

import Data.String
import qualified Data.List as List
import Control.Monad.State (State, gets, modify, evalState)

import Syntax
import Id
import Type
import Inst
import SSA hiding (M)
import AsmHelper

import qualified Data.Map as Map

data Env = Env { labelMap :: !(Map.Map BlockID Label), blkIdx :: !Int, currentFunction :: !SSAFundef } 
data ParCopy = ParCopy ![VId] ![Operand]

type M = State Env
runM :: M a -> a
runM x = evalState x (Env Map.empty 0 (SSAFundef (LId "undefined><><><" :-: TUnit) [] [] []))

data TailInfo = Tail | NonTail !(Maybe VId)

-- | Converts SSAFundef to instructions
emit :: [SSAFundef] -> [ZekInst]
emit fundefs = runM $ do
  sub <- fmap concat $ mapM emitFundef fundefs
  let prologue = li32 0x3000 rsp ++ li32 0x7000 rhp
  let endLabel = "min_caml_end"
  let epilogue = [ExtFile "zekamashi/libmincaml.txt", Label endLabel, Br rtmp endLabel]
  return $ prologue ++ [Bsr rlr "main", Br rtmp endLabel] ++ sub ++ epilogue


emitFundef :: SSAFundef -> M [ZekInst]
emitFundef fundef@(SSAFundef (LId nm :-: _ty) _params _formFV blks) = do
  modify $ \s -> s { currentFunction = fundef }
  entryLabel <- freshLabel "entry"
  res <- fmap concat $ mapM emitBlock blks
  return $ [Label nm, Br rtmp entryLabel] ++ res

emitBlock :: Block -> M [ZekInst]
emitBlock (Block blkId _phi insts term) = do
  lbl <- freshLabel blkId
  ii <- fmap concat $ mapM emitInst insts
  ti <- emitTerm blkId term
  return $ [Label lbl] ++ ii ++ ti

emitInst :: Inst -> M [ZekInst]
emitInst (Inst dest op) = emitSub (NonTail dest) op

emitTerm :: BlockID -> Term -> M [ZekInst]
emitTerm _ (TRet (OpConst UnitConst)) =
  return [Ret rtmp rlr]
emitTerm _ (TRet v) = do
  sub <- emitSub Tail (SId v)
  return $ sub ++ [Ret rtmp rlr]
emitTerm blkFrom (TJmp blkTo) = do
  pc <- findPhiCopy blkFrom blkTo
  l1 <- freshLabel blkTo
  return $ emitParCopy pc ++ [Br rtmp l1]
emitTerm blkFrom (TBr (OpVar (VId src :-: _ty)) blk1 blk2)
  = do
  l1 <- freshLabel blk1
  l2 <- freshLabel blk2
  pc1 <- findPhiCopy blkFrom blk1
  pc2 <- findPhiCopy blkFrom blk2
  fLabel <- freshLabel ("tbr." ++ blkFrom)
  return $ [ BC NE (regOfString src) fLabel
    ] ++ emitParCopy pc2 ++
    [ Br rtmp l2
    , Label fLabel
    ] ++ emitParCopy pc1 ++
    [ Br rtmp l1
    ] {- Not confirmed -}


-- | Emit code corresponding to the given Op.
emitSub :: TailInfo -> Op -> M [ZekInst]
emitSub _ (SCall (LId "@store" :-: _) [OpVar (VId v :-: _), OpConst (IntConst i)] _) =
  return [Stl (regOfString v) (fromIntegral i) rsp] 
emitSub (NonTail (Just (VId nm))) (SCall (LId "@load" :-: _) [OpConst (IntConst i)] _) =
  return [Ldl (regOfString nm) (fromIntegral i) rsp]
-- function call (tailcall/call with no result)
emitSub Tail (SCall (LId lid :-: _ty) ops _) = return $ emitArgs [] ops ++ [Br rlr lid]
emitSub (NonTail Nothing) (SCall (LId lid :-: _ty) ops st) =
  return $ 
    saveRegs ris ++
    emitArgs [] ops ++
    [Lda rsp (st + length regs) rsp] ++
    [Bsr rlr lid] ++
    [Lda rsp (- (st + length regs)) rsp] ++
    restoreRegs ris
  where regs = AsmHelper.gregs ++ [rlr]
        ris = zip regs [st..]
emitSub (NonTail Nothing) _ = return [] -- if not SCall there is no side-effect.
emitSub (NonTail (Just (VId nm))) o@(SCall (LId lid :-: _ty) ops st)
  | typeOfOp o == TFloat =
  let q = emitArgs [] ops in
  return $ q ++ [Bsr rlr lid] ++ fmov (FReg 0) (fregOfString nm)
emitSub (NonTail (Just (VId nm))) (SCall (LId lid :-: _ty) ops st) =
  let q = emitArgs [] ops in
  return $
    saveRegs ris ++
    q ++
    [Lda rsp (st + length regs) rsp] ++
    [Bsr rlr lid] ++ cp "$0" nm ++
    [Lda rsp (- (st + length regs)) rsp] ++
    restoreRegs (filter (\(a, _) -> a /= regOfString nm) ris)
  where regs = AsmHelper.gregs ++ [rlr]
        ris = zip regs [st..]
-- SId
emitSub (NonTail (Just (VId nm))) (SId (OpVar (VId src :-: ty))) =
  case ty of
    TFloat -> return $ fmov (fregOfString src) (fregOfString nm)
    _      -> return $ cp src nm
emitSub (NonTail (Just (VId nm))) (SId (OpConst cnst)) =
    case cnst of
      IntConst x   -> return $ li32 (fromIntegral x) (regOfString nm)
      FloatConst x -> return $ lfi (realToFrac x) (fregOfString nm)
      UnitConst    -> return []
-- Arithmetic operations
emitSub (NonTail (Just (VId nm))) (SArithBin aop (OpVar (VId src :-: _ty)) o2@(OpVar (VId _src2 :-: _ty2))) =
  let ctor = case aop of
        Add -> Addl
        Sub -> Subl
        Mul -> undefined
        Div -> undefined
  in return [ctor (regOfString src) (regimmOfOperand o2) (regOfString nm)]
emitSub (NonTail (Just (VId nm))) (SArithBin aop (OpVar (VId src :-: _)) (OpConst (IntConst imm))) =
  let val = case aop of
        Add -> imm
        Sub -> - imm
        Mul -> undefined
        Div -> undefined
  in return $ abstAdd (regOfString src) (fromIntegral val) (regOfString nm)
-- Negation (-x)
emitSub (NonTail (Just (VId nm))) (SNeg o@(OpVar (VId _ :-: _))) =
  return [Subl (Reg 31) (regimmOfOperand o) (regOfString nm)]
emitSub (NonTail (Just (VId nm))) (SNeg (OpConst cval)) =
  case cval of
    IntConst i -> emitSub (NonTail (Just (VId nm))) (SId (OpConst (IntConst (- i))))
    _          -> error $ "SNeg for non-int constant: " ++ show cval 
-- float binary operation
emitSub (NonTail (Just (VId nm))) (SFloatBin bop (OpVar (VId src1 :-: _)) (OpVar (VId src2 :-: _))) =
  let ctor = case bop of
        FAdd -> FOpAdd
        FSub -> FOpSub
        FMul -> FOpMul
        FDiv -> undefined
  in
    return $ [FOp ctor (fregOfString src1) (fregOfString src2) (fregOfString nm)]
emitSub (NonTail (Just (VId nm))) (SFloatBin bop (OpVar (VId src1 :-: _)) (OpConst (FloatConst fv))) =
  let ctor = case bop of
        FAdd -> FOpAdd
        FSub -> FOpSub
        FMul -> FOpMul
        FDiv -> undefined
  in
    return $ lfi (realToFrac fv) frtmp ++ [FOp ctor (fregOfString src1) frtmp (fregOfString nm)]
emitSub (NonTail (Just (VId nm))) (SFloatBin bop (OpConst (FloatConst fv)) (OpVar (VId src2 :-: _))) =
  let ctor = case bop of
        FAdd -> FOpAdd
        FSub -> FOpSub
        FMul -> FOpMul
        FDiv -> undefined
  in
    return $ lfi (realToFrac fv) frtmp ++ [FOp ctor frtmp (fregOfString src2) (fregOfString nm)]
-- float comparison
emitSub (NonTail (Just (VId nm))) (SCmpBin cop o@(OpVar (VId src1 :-: _)) (OpVar (VId src2 :-: _)))
  | getType o == TFloat =
  let ctor = case cop of
        Syntax.Eq -> CEQ
        Syntax.LE -> CLE
  in
    return $ [Cmps ctor (fregOfString src1) (fregOfString src2) (fregOfString nm)] -- TODO nm is integral register
-- integer comparison
emitSub (NonTail (Just (VId nm))) (SCmpBin cop (OpVar (VId src :-: _)) op2) =
  let ctor = case cop of
        Syntax.Eq -> CEQ
        Syntax.LE -> CLE
  in
    return $ [Inst.Cmp ctor (regOfString src) (regimmOfOperand op2) (regOfString nm)]
emitSub (NonTail (Just (VId nm))) (SCmpBin Syntax.Eq op1 (OpVar (VId src :-: _))) =
  return $ [Inst.Cmp CEQ (regOfString src) (regimmOfOperand op1) (regOfString nm)]
emitSub (NonTail (Just (VId nm))) (SCmpBin Syntax.LE op1 (OpVar (VId src :-: _))) =
  let condReg = regOfString nm in
  return $ [ Inst.Cmp CLT (regOfString src) (regimmOfOperand op1) condReg
  ] ++ abstAdd condReg (-1) condReg

emitSub Tail e@(SId op) = emitSub (NonTail (Just (retReg (getType op)))) e
emitSub Tail e@(SArithBin {}) = emitSub (NonTail (Just (retReg TInt))) e
emitSub Tail e@(SCmpBin {}) = emitSub (NonTail (Just (retReg TInt))) e
emitSub Tail e@(SFNeg {}) = emitSub (NonTail (Just (retReg TFloat))) e
emitSub Tail e@(SFloatBin {}) = emitSub (NonTail (Just (retReg TFloat))) e
emitSub Tail e@(SNeg {}) = emitSub (NonTail (Just (retReg TInt))) e
emitSub (NonTail _x) y = error $ "undefined behavior in emitSub: " ++ show y 

emitParCopy :: ParCopy -> [ZekInst]
emitParCopy (ParCopy var col) =
  let (ys, zs) = List.partition (\(_, x) -> getType x /= TFloat) (zip var col) in
  let yrs = [
        (ysrc, OpVar (ydest :-: getType ysrc)) |
        (ydest, ysrc) <- ys] in
  let gprs = List.concatMap
        (\ (y, r) -> movOperand y r)
        (shuffle (operandOfReg rtmp) yrs) in
  let zfrs = [
        (zsrc, OpVar (zdest :-: getType zsrc)) |
        (zdest, zsrc) <- zs] in
  let fregs = List.concatMap
        (\ (z, fr) -> fmovOperand z fr)
        (shuffle (OpVar (VId (show frtmp) :-: TFloat)) zfrs) in
    gprs ++ fregs


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
  let (_, yrs) = List.foldl'
        (\(i, yrs) y -> (i + 1, (y, OpVar (VId (show (Reg i)) :-: getType y)) : yrs))
        (0, map (\(x, reg_cl) -> (operandOfReg x, operandOfReg reg_cl)) x_reg_cl) ys in
  let gprs = List.concatMap
        (\ (y, r) -> movOperand y r)
        (shuffle (operandOfReg rtmp) yrs) in
  let (_, zfrs) = List.foldl'
        (\(d, zfrs) z -> (d + 1, (z, OpVar (VId (show (FReg d)) :-: getType z)) : zfrs))
        (0, []) zs in
  let fregs = List.concatMap
        (\ (z, fr) -> fmovOperand z fr)
        (shuffle (OpVar (VId (show frtmp) :-: TFloat)) zfrs) in
    gprs ++ fregs
-- helper functions

-- GPR: $0 ~ $31 ($31 = 0)
-- Float: $f0 ~ $f31

{- 関数呼び出しのために引数を並べ替える (register shuffling) -}
shuffle :: Operand -> [(Operand, Operand)] -> [(Operand, Operand)]
shuffle sw xys =
  let (xys0, imm) = List.partition (\ (_, y) -> case y of { OpVar {} -> True; _ -> False; }) xys in
  {- remove identical moves -}
  let xys1 = List.filter (\ (x, y) -> x /= y) xys0 in
  {- find acyclic moves -}
  let sub1 = case List.partition (\ (_, y) -> List.lookup y xys1 /= Nothing) xys1 of
            ([], []) -> []
            ((x, y) : xys_rest, []) -> {- no acyclic moves; resolve a cyclic move -}
                (y, sw) : (x, y) :
                  shuffle sw (List.map (\e -> case e of
                                    (y', z) | y == y' -> (sw, z)
                                    yz -> yz) xys_rest)
            (xys', acyc) -> acyc ++ shuffle sw xys'
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
regimmOfOperand (OpConst y) = error $ "regimmOfOperand: invalid argument: " ++ show y
regimmOfOperand (OpVar (VId src :-: _)) = case regOfString src of Reg x -> RIReg x

freshLabel :: BlockID -> M Label
freshLabel blkId = do
  LId f :-: _ <- gets (SSA.name . currentFunction)
  let uid = f ++ "." ++ blkId
  lm <- gets labelMap
  if Map.member uid lm then
    return $ lm Map.! uid
  else do
    x <- gets blkIdx
    let str = uid ++ "." ++ show x 
    modify $ \s -> s { labelMap = Map.insert uid str lm, blkIdx = x + 1 }
    return str

findPhiCopy :: BlockID -> BlockID -> M ParCopy
findPhiCopy blkFrom blkTo = do
  curFun <- gets currentFunction
  let Block _ (Phi vars cols) _ _ = getBlockByID curFun blkTo
  return $ ParCopy vars (cols Map.! blkFrom)

operandOfReg :: Reg -> Operand
operandOfReg x = OpVar (VId (show x) :-: TInt)


movOperand :: Operand -> Operand -> [ZekInst]
movOperand (OpVar (a :-: _)) (OpVar (b :-: _)) = mov (regOfString a) (regOfString b)
movOperand (OpConst (IntConst v)) (OpVar (b :-: _)) = li32 (fromIntegral v) (regOfString b)
movOperand (OpConst UnitConst) (OpVar (b :-: _)) = []
movOperand x y = error $ "movOperand: invalid arguments: " ++ show x ++ ", " ++ show y

fmovOperand :: Operand -> Operand -> [ZekInst]
fmovOperand (OpVar (a :-: _)) (OpVar (b :-: _)) = fmov (fregOfString a) (fregOfString b)
fmovOperand (OpConst (FloatConst v)) (OpVar (b :-: _)) = lfi (realToFrac v) (fregOfString b)
fmovOperand x y = error $ "fmovOperand: invalid arguments: " ++ show x ++ ", " ++ show y

saveRegs :: [(Reg, Int)] -> [ZekInst]
saveRegs regPls = map (\(x, y) -> Stl x y rsp) regPls

restoreRegs :: [(Reg, Int)] -> [ZekInst]
restoreRegs regPls = map (\(x, y) -> Ldl x y rsp) regPls

