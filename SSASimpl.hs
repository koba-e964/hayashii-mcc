module SSASimpl where

import Id
import SSA
import Type
import Syntax

simplFundef :: SSAFundef -> SSAFundef
simplFundef fundef@(SSAFundef {blocks = blks} ) =
  fundef { blocks = simplBlocks blks }

simplBlocks :: [Block] -> [Block]
simplBlocks blocks = f where
  f = blocks



{- Simplifies the CFG. -}
simplify :: [SSAFundef] -> [SSAFundef]
simplify = map simplFundef
