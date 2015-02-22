module SSALiveness where

import qualified Data.List as List
import qualified Data.Map as Map
import Data.Set (Set, union, unions)
import qualified Data.Set as Set

import Id
import Type
import SSA

genPhi :: Phi -> BlockID -> Set VId
genPhi (Phi _ cols) blk = Set.unions $ List.map genOperand $ cols Map.! blk

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

killPhi :: Phi -> Set VId
killPhi (Phi vars _) = Set.fromList vars

killInst :: Inst -> Set VId
killInst (Inst (Just v) _) = Set.singleton v
killInst (Inst Nothing _) = Set.empty

newtype LiveInfo = LiveInfo (Map.Map BlockID BlockLive) deriving (Eq)
data BlockLive = BlockLive !PhiLive ![InstLive] !TermLive deriving (Eq)
type PhiLive = Map.Map BlockID InstLive
data InstLive = InstLive { liveIn :: !(Set VId), liveOut :: !(Set VId) } deriving (Eq)
type TermLive = InstLive

instance Show InstLive where
  show (InstLive i o) = "in=" ++ f i ++ ", out=" ++ f o where
    f liveset = show (Set.toList liveset)
instance Show BlockLive where
  show (BlockLive phil instl terml) = List.intercalate "\n" (show phil : map show (instl ++ [terml]))
instance Show LiveInfo where
  show (LiveInfo info) = List.intercalate "\n" (map (\i -> show i ++ "\n") $ Map.toList info)

nextOfTerm :: Term -> [BlockID]
nextOfTerm (TRet _) = []
nextOfTerm (TBr _ blk1 blk2) = [blk1, blk2]
nextOfTerm (TJmp blk) = [blk]

prevOfPhi :: Phi -> [BlockID]
prevOfPhi (Phi _ cols) = Map.keys cols

nextSets :: SSAFundef -> LiveInfo -> LiveInfo
nextSets (SSAFundef _ _ _ blks) (LiveInfo linfo) =
  LiveInfo $ Map.fromList $ map (g linfo) blks where
  g info (Block blk phi insts term) =
    let (BlockLive phil instl terml) = info Map.! blk in
    let len = length insts in
    let newPhiIn blkID = (liveOut (phil Map.! blkID) `Set.difference` killPhi phi) `union` genPhi phi blkID in
    let newPhiOut = liveIn ((instl ++ [terml]) !! 0) in
    let newIn i = (liveOut (instl !! i) `Set.difference` killInst (insts !! i)) `union` genInst (insts !! i) in
    let newOut i = liveIn ((instl ++ [terml]) !! (i + 1)) in
    let termIn = liveOut terml `union` genTerm term in
    let termOut = unions $ map (\blkID -> let BlockLive phil' _ _ = info Map.! blkID in liveIn (phil' Map.! blk)) (nextOfTerm term) in
    (blk, BlockLive (Map.fromList [(i, InstLive (newPhiIn i) newPhiOut) | i <- Map.keys phil]) [InstLive (newIn i) (newOut i) | i <- [0 .. len - 1]] (InstLive termIn termOut))

analyzeLiveness :: SSAFundef -> LiveInfo
analyzeLiveness fundef@(SSAFundef _ _ _ blks) = minFix (nextSets fundef) w where
  w = LiveInfo $ Map.fromList $ map g blks where
  g (Block blk phi insts _) =
    let len = length insts in
    let emp = InstLive Set.empty Set.empty in
    let phil = Map.fromList ([(i, emp) | i <- prevOfPhi phi]) in
    let instl = replicate len emp in
    (blk, BlockLive phil instl emp)

type Interference = Map.Map VId (Set VId)

emptyInterference :: Interference
emptyInterference = Map.empty

unionInt :: Interference -> Interference -> Interference
unionInt i1 i2 = Map.unionWith Set.union i1 i2

accLiveInfo :: LiveInfo -> Interference
accLiveInfo (LiveInfo info) = Map.foldl' (\x y -> x `unionInt` accBlockLive y) emptyInterference info

accBlockLive :: BlockLive -> Interference
accBlockLive (BlockLive phil instl terml) = Map.foldl' (\x y -> x `unionInt` accInstLive y) emptyInterference phil `unionInt`
  (List.foldl' (\x y -> x `unionInt` accInstLive y) emptyInterference instl) `unionInt` accInstLive terml

accInstLive :: InstLive -> Interference
accInstLive (InstLive i o) = f i `unionInt` f o where
  f set = Map.fromSet (\x -> set `Set.difference` Set.singleton x) set



