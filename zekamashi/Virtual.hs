module Virtual where

import SSA
import Id
import Type
import AsmHelper

virtualCode :: SSAFundef -> VirFunc
virtualCode (SSAFundef (lid :-: _) _ _ blks) = VirFunc lid (virtualBlock blks)

virtualBlock :: [Block] -> [VirBlock]
virtualBlock blks = map f blks
  where
  f (Block blkId phi insts term) = VirBlock blkId []
 

