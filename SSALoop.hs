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
      newBlocks = newEntry : map f blks -- adds new entry block
      newArgs = map (SSALoop.freshVar (varsFundef fundef)) oldArgs -- Rename parameters
      newArgOps = map OpVar newArgs :: [Operand]
      tailCalls = catMaybes (map (isTailCall lid) blks) :: [(BlockID, [Operand])] -- Blocks which contain tail call.
      oldEntryID = freshString (map blockID blks) "e" :: BlockID
      renamer = (\b -> if b == "entry" then oldEntryID else b)
      f (Block blkID phi insts term) =
        let (nb, np) = g blkID phi
            (ni, nt) = h blkID insts term in
        Block nb np ni nt
      g blkID phi | blkID == "entry" = (oldEntryID, Phi (map unType oldArgs) ((Map.singleton "entry" newArgOps `Map.union` renameCols renamer (Map.fromList tailCalls)))) -- The old entry block has to be renamed.
      g blkID (Phi vars cols) = (blkID, Phi vars (renameCols renamer cols))
      h blkID insts term | elem blkID (map fst tailCalls) = (init insts, TJmp oldEntryID) -- Tail call has to be replaced with jump to the old entry block
      h _ insts term = (insts, renameTerm renamer term)
      x = if null tailCalls then fundef else fundef { args = newArgs, blocks = newBlocks } in
  x


renameTerm :: (BlockID -> BlockID) -> Term -> Term
renameTerm f (TRet x) = TRet x
renameTerm f (TJmp x) = TJmp (f x)
renameTerm f (TBr x b1 b2) = TBr x (f b1) (f b2)

renameCols :: (BlockID -> BlockID) -> Map.Map BlockID [Operand] -> Map.Map BlockID [Operand]
renameCols f cols = Map.mapKeys f cols

-- If given block has tail call to itself, this function returns the argument wrapped with Just. Otherwise, this returns Nothing.
isTailCall :: LId -> Block -> Maybe (BlockID, [Operand])
isTailCall caller (Block blkID _phi insts term) = if length insts == 0 then Nothing else
    case (last insts, term) of
      (Inst dest (SCall (f :-: _) op _), TRet res) | f == caller -> case (dest, res) of
         (Just d, OpVar (r :-: _)) | d == r -> Just (blkID, op)
         (_, OpConst UnitConst) -> Just (blkID, op)
         _                      -> Nothing
      _  -> Nothing

-- [freshString ss prefix] returns a [String] that has [prefix] as prefix and is not contained in [ss].
freshString :: [String] -> String -> String
freshString ss prefix = head $ filter (flip notElem ss) $ [prefix ++ "." ++ show i | i <- [0 .. ]]


freshVar :: [VId] -> Typed VId -> Typed VId
freshVar ss (VId prefix :-: ty) = VId (freshString (map (\(VId x) -> x) ss) prefix) :-: ty

unType :: Typed a -> a
unType (x :-: _) = x
