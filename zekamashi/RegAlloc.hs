module RegAlloc where

import Control.Applicative
import Control.Monad.State
import Data.Bits
import Data.Maybe
import Data.Word
import qualified Data.Map as Map
import SSA hiding (M)
import Id

data RegEnv = RegEnv { regmap :: !(Map.Map String Loc), gregs :: !Word32, fregs :: !Word32, stack :: !Int }
emptyEnv :: RegEnv
emptyEnv = RegEnv Map.empty 0 0 0
type M = State RegEnv
data Loc = Reg !Int | Stack !Int deriving (Eq)

instance Show Loc where
  show (Reg v) = '$' : show v
  show (Stack i) = "st" ++ show i

regAlloc :: [SSAFundef] -> [SSAFundef]
regAllocBlocks :: [Block] -> [Block]
rab :: [Block] -> M [Block]


regAlloc = map regAllocFundef where
  regAllocFundef (SSAFundef nty params formFV blks) =
    SSAFundef nty params formFV (regAllocBlocks blks)

regAllocBlocks blk = evalState (rab blk) emptyEnv

rab blk = do
  let vars = concatMap varsBlock blk
  _locs <- mapM newReg vars {- TODO ignoring instructions -}
  mapM replace blk

varsBlock :: Block -> [VId]
varsBlock (Block blkId insts term) = concatMap f insts where
  f (Inst Nothing _) = []
  f (Inst (Just v) _) = [v]

replace (Block blkId insts term) = Block blkId <$> mapM replaceInst insts <*> replaceTerm term

replaceInst (Inst mvid op) = do
  newMvid <- case mvid of
    Nothing -> return Nothing
    Just v -> Just <$> getLoc v
  Inst newMvid <$> replaceOp op


replaceOp op = return op
replaceTerm term = return term

getLoc :: VId -> M VId
getLoc (VId nm) = do
  x <- gets (fromJust . Map.lookup nm . regmap)
  return $! VId $! show x
newReg :: VId -> M Loc
newReg (VId vname) = do
  RegEnv rmap gr fr st <- get
  let vac = complement gr
  if vac == 0 then do
    let sst = Stack st
    modify $ \s -> s { regmap = Map.insert vname sst rmap, stack = st + 1 }
    return $! sst
  else do
    let min = vac .&. (- vac)
    let num = popCount (min - 1)
    let reg = Reg num
    modify $ \s -> s { regmap = Map.insert vname reg rmap, gregs = gr .|. min }
    return $! reg
freeReg :: Loc -> M ()
freeReg v = undefined
