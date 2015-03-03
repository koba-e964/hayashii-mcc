module SSADom where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List

import SSA
import Graph

type Dom = Graph BlockID
-- | emptyDom represents the initial dominance relation of equation, in which every dominance holds.
emptyDom :: SSAFundef -> Dom
emptyDom SSAFundef { blocks = blks } =
  let blkIDs = [bid | b <- blks, let bid = blockID b] in
  let blkSet = Set.fromList blkIDs in
  Map.fromList [(bid, blkSet) | bid <- blkIDs] -- complete graph, with self loops

nextDom :: SSAFundef -> Dom -> Dom

nextDom SSAFundef { blocks = blks } oldDom = Map.fromList $
  [("entry", Set.singleton "entry")] ++
  [(bid, e `Set.union` Set.singleton bid) | b <- blks, let bid = blockID b, bid /= "entry", let e = foldl1' Set.intersection (map (oldDom Map.!) $ prevBlocks b)]

-- | The maximum solution of the equation of dominance.
dominance :: SSAFundef -> Dom
dominance fundef = minFix (nextDom fundef) (emptyDom fundef)
