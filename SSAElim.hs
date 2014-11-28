module SSAElim where

import SSA
import Id
import Type

import qualified Data.Set as Set

{- for every instruction x := op, if @x@ does not appear in right-side, assignment to @x@ is eliminated. -}
elimDest :: SSAFundef -> SSAFundef
elimDest fundef@(SSAFundef {blocks = blks} ) =
  let appearance = concat (map fvBlock blks) in
  fundef { blocks = map (removeDest appearance) blks }


fvBlock :: Block -> [VId]
fvBlock (Block _blkId insts term) = concatMap (\(Inst _ op) -> fvOp op) insts ++ fvTerm term

fvOp :: Op -> [VId]
fvOp op = case op of
  SId x -> fvOperand x
  SNeg x -> fvOperand x
  SFNeg x -> fvOperand x
  SArithBin _ x y -> fvOperand x ++ fvOperand y
  SCmpBin _ x y -> fvOperand x ++ fvOperand y
  SFloatBin _ x y -> fvOperand x ++ fvOperand y
  SCall _ args -> concatMap fvOperand args
  SPhi ls -> concatMap (fvOperand . snd) ls

fvTerm :: Term -> [VId]
fvTerm (TRet op) = fvOperand op
fvTerm (TBr op _ _) = fvOperand op
fvTerm (TJmp {}) = []

fvOperand :: Operand -> [VId]
fvOperand (OpVar (vid :-: _)) = [vid]
fvOperand _ = []


removeDest :: [VId] -> Block -> Block
removeDest app (Block blkId insts term) = Block blkId (map f insts) term
  where
    f (Inst (Just vid) op) | notElem vid app = Inst Nothing op
    f e = e

elimUselessInstructions :: SSAFundef -> SSAFundef
elimUselessInstructions fundef@(SSAFundef {blocks = blks} ) =
  fundef { blocks = map removeUselessInstructions blks }

removeUselessInstructions :: Block -> Block
removeUselessInstructions (Block blkId insts term) = Block blkId (filter f insts) term
  where
    f (Inst Nothing op) = case op of
      SId {} -> False
      SNeg {} -> False
      SFNeg {} -> False
      SArithBin {} -> False
      SFloatBin {} -> False
      SCmpBin {} -> False
      SPhi {} -> False
      _ -> True
    f _ = True

elimUnreachableBlocks :: SSAFundef -> SSAFundef
elimUnreachableBlocks fundef@(SSAFundef {blocks = blks} ) =
  fundef { blocks = f } where
    cfgDest (Block _ _ term) = case term of
      TRet {} -> []
      TBr _ blk1 blk2 -> [blk1, blk2]
      TJmp b -> [b]
    reachable = Set.fromList $ entryBlockName : concatMap cfgDest blks {- TODO Super-tenuki. Ideally, this should be checked by reachablity analysis from entry. -}
    f = filter (\(Block blkID _ _) -> Set.member blkID reachable) blks




{- perform dead code elimination -}
eliminate :: [SSAFundef] -> [SSAFundef]
eliminate = map f where
   f e = let e' = elimUselessInstructions (elimUnreachableBlocks (elimDest e)) in
    if e == e' then e else f e'

