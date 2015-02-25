module SSAReduce where

import SSA
import Syntax
import qualified Data.Map as Map

reduceFundef :: SSAFundef -> SSAFundef
reduceFundef fundef@(SSAFundef {blocks = blks} ) =
  mapEndoBlock (reduceBlock (map blkID blks)) fundef
  where blkID (Block x _ _ _) = x

reduceBlock :: [BlockID] -> Block -> Block
reduceBlock blkIDs (Block blkId phi insts term) = Block blkId (f phi) (map g insts) (h term)
  where
    f (Phi vars cols) =
        let meanful = Map.filterWithKey (\x _ -> elem x blkIDs) cols in
        Phi vars meanful 
    g (Inst dest op) = Inst dest $ case op of
      SArithBin Add x (OpConst (IntConst 0)) -> SId x
      SArithBin Sub x (OpConst (IntConst 0)) -> SId x
      SArithBin Mul _ (OpConst (IntConst 0)) -> SId (OpConst (IntConst 0))
      SArithBin Mul x (OpConst (IntConst 1)) -> SId x
      SArithBin Mul x (OpConst (IntConst 2)) -> SArithBin Add x x
      SArithBin Add (OpConst t) (OpVar x) -> SArithBin Add (OpVar x) (OpConst t)
      SArithBin Mul (OpConst t) (OpVar x) -> SArithBin Mul (OpVar x) (OpConst t)
      e -> e
    h (TBr _ blk1 blk2) | blk1 == blk2 = TJmp blk1
    h (TBr (OpConst (IntConst x)) blk1 blk2) = case x of
      1 -> TJmp blk1
      0 -> TJmp blk2
      _ -> error $ "condition must be 0 or 1, but got:" ++ show x
    h t = t


{- Performs operator strength reduction -}
reduce :: [SSAFundef] -> [SSAFundef]
reduce = map reduceFundef
