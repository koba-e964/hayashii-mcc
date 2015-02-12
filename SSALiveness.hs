module SSALiveness where

import Data.Array.IArray
import Data.Set (Set, union, unions)
import qualified Data.Set as Set

import Id
import Type
import SSA


genInst :: Inst -> Set VId
genInst (Inst _ o) = genOp o

genOp :: Op -> Set VId
genOp e = case e of
  SId o -> genOperand o
  SArithBin _ o1 o2 -> genOperand o1 `union` genOperand o2
  SFloatBin _ o1 o2 -> genOperand o1 `union` genOperand o2
  SCmpBin _ o1 o2 -> genOperand o1 `union` genOperand o2
  SNeg o -> genOperand o
  SFNeg o -> genOperand o
  SCall _ ls -> unions (map genOperand ls)
  SPhi ls -> unions $ map (\(_, o) -> genOperand o) ls

genOperand :: Operand -> Set VId
genOperand (OpVar (v :-: _)) = Set.singleton v
genOperand (OpConst _) = Set.empty

genTerm :: Term -> Set VId
genTerm (TRet o) = genOperand o
genTerm (TBr o _ _) = genOperand o
genTerm (TJmp _) = Set.empty

killInst :: Inst -> Set VId
killInst (Inst (Just v) _) = Set.singleton v
killInst (Inst Nothing _) = Set.empty

nextSets :: SSAFundef -> [(BlockID, ([Set VId], [Set VId]))] -> [(BlockID, ([Set VId], [Set VId]))]
nextSets (SSAFundef _ args_ _ blks) ls =
  map (g ls) blks where
  g ls (Block blk insts term) =
    let Just (setsIn, setsOut) = lookup blk ls in
    let len = length insts in
    let newIn = [(setsIn !! i) `union` genInst (insts !! i) | i <- [0 .. len - 1]] ++ [(setsIn !! len) `union` genTerm term] in
    let newOut = [setsIn !! (i + 1) | i <- [0 .. len - 1]] in
    (blk, (newIn, newOut))

minFix :: Eq q => (q -> q) -> q -> q
minFix f init = let e = f init in
  if e == init then init else minFix f e

analyzeLiveness :: SSAFundef -> [(BlockID, ([Set VId], [Set VId]))]
analyzeLiveness fundef@(SSAFundef _ args_ _ blks) = minFix (nextSets fundef) w where
  w = map g blks
  g (Block blk insts term) =
    let len = length insts in
    let newIn = replicate (len + 1) Set.empty in
    let newOut = replicate len Set.empty in
    (blk, (newIn, newOut))




