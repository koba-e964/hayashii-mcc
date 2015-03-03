module SSACSE where

import SSA
import SSADom
import Id
import Type
import qualified Data.Map as Map
import qualified Data.Set as Set


newtype Env = Env { defMap :: Map.Map VId (BlockID, Op) } 

defEq :: Op -> Op -> Bool
defEq (SCall {}) (SCall {}) = False
defEq x y = x == y


-- | Performs common subexpression elimination (CSE).
cse :: [SSAFundef] -> [SSAFundef]
cse = map f where
  f fundef = let cinfo = collectInfo fundef in unifyDefs cinfo fundef


-- | Collects definition info from fundef.
collectInfo :: SSAFundef -> Env
collectInfo SSAFundef { blocks = blks } = Env $ Map.fromList $ concatMap f blks where
  f :: Block -> [(VId, (BlockID, Op))]
  f (Block blk _phi insts _term) = concatMap (g blk) insts
  g :: BlockID -> Inst -> [(VId, (BlockID, Op))]
  g _blk (Inst Nothing _) = []
  g blk (Inst (Just dest) op) = [(dest, (blk, op))]

-- | Unify multiple definitions according to domination relations and defEq.
-- | For every two definitions a := b, c := d, if b `defEq` d and block(a) dominates block(c), c := d can be safely replaced with c := a.
unifyDefs :: Env -> SSAFundef -> SSAFundef
unifyDefs env fundef = 
  let dom = dominance fundef in
  mapEndoBlock (f dom) fundef
  where
  f :: Dom -> Block -> Block
  f dom (Block blkID phi insts term) = Block blkID phi [g (concatMap varInst $ take i insts) (insts !! i) | i <- [0 .. length insts - 1]] term where
    g _vids i@(Inst Nothing _) = i
    g vids (Inst (Just d) op) = case searchDef env dom blkID vids d op of
      Nothing  -> Inst (Just d) op
      Just vid -> Inst (Just d) (SId (OpVar (vid :-: typeOfOp op)))


-- | Search for the definition x := op2 in block bid s.t. op `defEq` op2 && bid `Set.member` dominator && .
searchDef :: Env -> Dom -> BlockID -> [VId] -> VId -> Op -> Maybe VId
searchDef (Env dm) dom blkDef curDef dest op = 
  let ok = Map.keys $ Map.filterWithKey p dm in
  case ok of
    [] -> Nothing
    (x : _) -> Just x
  where
    p vid (bid, op2) | bid == blkDef = op `defEq` op2 && elem vid curDef
    p vid (bid, op2) = Set.member bid (dom Map.! blkDef) && op `defEq` op2

