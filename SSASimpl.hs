module SSASimpl where

import Id
import SSA
import Type
import Syntax
import qualified Data.Map as Map
import Control.Monad.Reader

import Data.Maybe

type Snippet = ([Inst], Term)
type BlockEnv = Map.Map BlockID Snippet

type M = Reader BlockEnv

simplFundef :: SSAFundef -> SSAFundef
simplFundef fundef@(SSAFundef {blocks = blks} ) =
  fundef { blocks = simplBlocks blks }

simplBlocks :: [Block] -> [Block]
simplBlocks blocks = f where
  ok blk@(Block blkID insts term) =
    if length insts >= 2 then Nothing else Just (blkID, (insts, term))
  env = Map.fromList $ catMaybes $ map ok blocks
  f = map (replace env) blocks



append :: Snippet -> Block -> Block
append (inst, term) (Block blkId insts _) = Block blkId (insts ++ inst) term


replace :: BlockEnv -> Block -> Block
replace env blk@(Block blkId insts term) =
  case term of
    TJmp dest | blkId /= dest {- to avoid recursion -} && Map.member dest env ->
      append (env Map.! dest) blk {- TODO variable renaming and phi elimination are not performed. -}
    _ -> blk
  


{- Simplifies the CFG. -}
simplify :: [SSAFundef] -> [SSAFundef]
simplify = map simplFundef
