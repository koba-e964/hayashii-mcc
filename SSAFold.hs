module SSAFold where

import SSA
import Syntax

constFold :: [SSAFundef] -> [SSAFundef]
constFold = map cfFundef


cfFundef :: SSAFundef -> SSAFundef
cfFundef = mapEndoBlock f
  where
    f (Block blkId phi insts term) = Block blkId phi (map g insts) term
    g (Inst dest op) = Inst dest $ case op of
      SNeg (OpConst (IntConst x)) -> SId (OpConst (IntConst (-x)))
      SArithBin operator (OpConst (IntConst x)) (OpConst (IntConst y)) ->
        SId $ OpConst $ IntConst $ case operator of
          Add -> x + y
          Sub -> x - y
          Mul -> x * y
          Div -> x `div` y
      SFNeg (OpConst (FloatConst x)) -> SId (OpConst (FloatConst (-x)))
      SFloatBin operator (OpConst (FloatConst x)) (OpConst (FloatConst y)) ->
        SId $ OpConst $ FloatConst $ case operator of
          FAdd -> x + y
          FSub -> x - y
          FMul -> x * y
          FDiv -> x / y
      SCmpBin operator (OpConst (IntConst x)) (OpConst (IntConst y)) ->
        SId $ OpConst $ IntConst $ case operator of
          Eq -> if x == y then 1 else 0
          LE -> if x <= y then 1 else 0
      SCmpBin operator (OpConst (FloatConst x)) (OpConst (FloatConst y)) ->
        SId $ OpConst $ IntConst $ case operator of
          Eq -> if x == y then 1 else 0
          LE -> if x <= y then 1 else 0
      _ -> op

