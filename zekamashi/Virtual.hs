module Virtual where

import SSA
import Inst




elimPhi :: [SSAFundef] -> [SSAFundef]
elimPhi = map elimPhiFundef


elimPhiFundef :: SSAFundef -> SSAFundef
elimPhiFundef fundef@(SSAFundef {blocks = blks} ) =
  fundef
