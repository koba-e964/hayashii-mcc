module SSALoop where
 
import SSA
import Id
import Type
import qualified Data.Map as Map
import Data.Maybe (catMaybes)

import Debug.Trace

-- | Convert recursion to loop.
convertToLoop :: SSAFundef -> SSAFundef
convertToLoop fundef@SSAFundef { name = lid :-: _, args = oldArgs, blocks = blks} = 
  let newEntry = Block "entry" emptyPhi [] (TJmp oldEntryID)
      oldEntryID = freshString (map blockID blks) "e" :: BlockID
      renamer b =　if b == "entry" then oldEntryID else b
      renamedBlocks = map (rename renamer) blks
      newBlocks = newEntry : map f renamedBlocks -- adds new entry block, renames "entry" with other fresh name and connects them.
      newArgs = map (SSALoop.freshVar (varsFundef fundef)) oldArgs -- Renames parameters
      newArgOps = map OpVar newArgs :: [Operand]
      tailCalls = catMaybes (map (isTailCall lid) renamedBlocks) :: [(BlockID, [Operand])] -- Blocks which contain tail call.
      f (Block blkID phi insts term) =
        let (nb, np) = g blkID phi
            (ni, nt) = h blkID insts term in
        Block nb np ni nt
      g blkID phi | blkID == oldEntryID = (oldEntryID, Phi (map unType oldArgs) ((Map.singleton "entry" newArgOps `Map.union`  Map.fromList tailCalls))) -- The old entry block has to be renamed.
      g blkID phi = (blkID, phi)
      h blkID insts term | elem blkID (map fst tailCalls) = (init insts, TJmp oldEntryID) -- Tail call has to be replaced with jump to the old entry block
      h _ insts term = (insts, term)
      x = if null tailCalls then fundef else fundef { args = newArgs, blocks = newBlocks } in
  x


-- If given block has tail call to itself, this function returns the argument wrapped with Just. Otherwise, this returns Nothing.
isTailCall :: LId -> Block -> Maybe (BlockID, [Operand])
isTailCall caller (Block blkID _phi insts term) = if length insts == 0 then Nothing else
    case (last insts, term) of
      (Inst dest (SCall (f :-: _) op _), TRet res) | f == caller -> case (dest, res) of
         (Just d, OpVar (r :-: _)) | d == r -> Just (blkID, op)
         (_, OpConst UnitConst) -> Just (blkID, op)
         _                      -> Nothing
      _  -> Nothing


freshVar :: [VId] -> Typed VId -> Typed VId
freshVar ss (VId prefix :-: ty) = VId (freshString (map (\(VId x) -> x) ss) prefix) :-: ty

