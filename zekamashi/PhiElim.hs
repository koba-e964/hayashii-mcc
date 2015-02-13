module PhiElim where

import SSA




elimPhi :: [SSAFundef] -> [SSAFundef]
elimPhi = map elimPhiFundef


elimPhiFundef :: SSAFundef -> SSAFundef
elimPhiFundef fundef@(SSAFundef {blocks = blks} ) =
  fundef
