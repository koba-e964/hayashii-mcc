module SSALiveness where

import Data.Array.IArray
import qualified Data.List as List
import qualified Data.Map as Map
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

newtype LiveInfo = LiveInfo (Map.Map BlockID BlockLive) deriving (Eq)
data BlockLive = BlockLive ![InstLive] !TermLive deriving (Eq)
data InstLive = InstLive { liveIn :: !(Set VId), liveOut :: !(Set VId) } deriving (Eq)
type TermLive = InstLive

instance Show InstLive where
  show (InstLive i o) = "in=" ++ f i ++ ", out=" ++ f o where
    f liveset = show (Set.toList liveset)
instance Show BlockLive where
  show (BlockLive instl terml) = concatMap (\i -> show i ++ "\n") (instl ++ [terml])
instance Show LiveInfo where
  show (LiveInfo info) = List.intercalate "\n" (map show $ Map.toList info)


-- nextSets :: SSAFundef -> [(BlockID, ([Set VId], [Set VId]))] -> [(BlockID, ([Set VId], [Set VId]))]
nextSets :: SSAFundef -> LiveInfo -> LiveInfo
nextSets (SSAFundef _ _ _ blks) (LiveInfo info) =
  LiveInfo $ Map.fromList $ map (g info) blks where
  g info (Block blk insts term) =
    let (BlockLive instl terml) = info Map.! blk in
    let len = length insts in
    let newIn i = (liveOut (instl !! i) `Set.difference` killInst (insts !! i)) `union` genInst (insts !! i) in
    let newOut i = liveIn ((instl ++ [terml]) !! (i + 1)) in
    let termIn = liveOut terml `union` genTerm term in
    let termOut = Set.empty {- not correct -} in
    (blk, BlockLive [InstLive (newIn i) (newOut i) | i <- [0 .. len - 1]] (InstLive termIn termOut))

minFix :: Eq q => (q -> q) -> q -> q
minFix f init = let e = f init in
  if e == init then init else minFix f e

analyzeLiveness :: SSAFundef -> LiveInfo
analyzeLiveness fundef@(SSAFundef _ args_ _ blks) = minFix (nextSets fundef) w where
  w = LiveInfo $ Map.fromList $ map g blks where
  g (Block blk insts term) =
    let len = length insts in
    let emp = InstLive Set.empty Set.empty in
    let instl = replicate len emp in
    (blk, BlockLive instl emp)




