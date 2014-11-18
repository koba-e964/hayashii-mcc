module SSAElim where

import SSA
import Id
import Type

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
  SFloatBin _ x y -> fvOperand x ++ fvOperand y
  SCall _ args -> concatMap fvOperand args

fvTerm :: Term -> [VId]
fvTerm (TRet op) = fvOperand op
fvTerm (TBr op _ _) = fvOperand op

fvOperand :: Operand -> [VId]
fvOperand (OpVar (vid :-: _)) = [vid]
fvOperand _ = []


removeDest :: [VId] -> Block -> Block
removeDest app (Block blkId insts term) = Block blkId (map f insts) term
  where
    f (Inst (Just vid) op) | notElem vid app = Inst Nothing op
    f e = e
