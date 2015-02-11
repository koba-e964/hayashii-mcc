{- One of the most naive register allocation -}
module RegAlloc where

import Control.Applicative
import qualified Control.Arrow
import Control.Monad.State
import Data.Bits
import Data.Maybe
import Data.Word
import qualified Data.Map as Map
import SSA hiding (M)
import Id
import Type

data RegEnv = RegEnv { regmap :: !(Map.Map String Loc), gregs :: !Word32, fregs :: !Word32, stack :: !Int }
emptyEnv :: RegEnv
emptyEnv = RegEnv Map.empty 0 0 0
type M = State RegEnv
data Loc = Reg !Int | FReg !Int | Stack !Int deriving (Eq)

instance Show Loc where
  show (Reg v) = '$' : show v
  show (FReg v) = "$f" ++ show v
  show (Stack i) = "st" ++ show i

regAlloc :: [SSAFundef] -> [SSAFundef]


regAlloc = map regAllocFundef where
  regAllocFundef fundef = evalState (rab fundef) emptyEnv

rab (SSAFundef nty params formFV blks) = do
  let vars = concatMap varsBlock blks
  forM_ params $ \(vid :-: ty) ->
    newReg (vid, ty) {- TODO ignoring type, should allocate fixed register -}
  mapM_ newReg vars {- TODO ignoring instructions -}
  newBlks <- mapM replace blks
  newParams <- forM params $ \(vid :-: ty) -> do
    l <- getLoc vid
    return $! l :-: ty
  return $! SSAFundef nty newParams formFV newBlks

varsBlock :: Block -> [(VId, Type)]
varsBlock (Block blkId insts term) = concatMap f insts where
  f (Inst Nothing _) = []
  f (Inst (Just v) op) = [(v, typeOfOp op)]

replace (Block blkId insts term) = Block blkId <$> mapM replaceInst insts <*> replaceTerm term

replaceInst (Inst mvid op) = do
  newMvid <- case mvid of
    Nothing -> return Nothing
    Just v -> Just <$> getLoc v
  Inst newMvid <$> replaceOp op


replaceOp op = do
  case op of
    SId c ->
      return $ SId c
    SArithBin operator x y ->
      SArithBin operator <$> replaceOperand x <*> replaceOperand y
    SCmpBin operator x y ->
      SCmpBin operator <$> replaceOperand x <*> replaceOperand y
    SNeg x ->
      SNeg <$> replaceOperand x
    SFNeg x ->
      SFNeg <$> replaceOperand x
    SFloatBin operator x y ->
      SFloatBin operator <$> replaceOperand x <*> replaceOperand y
    SCall lid operands ->
      SCall lid <$> mapM replaceOperand operands
    SPhi ls ->
      let f (x, y) = do { r <- replaceOperand y; return (x, r);} in
      SPhi <$> mapM f ls

replaceOperand op = case op of
  OpConst _ -> return op
  OpVar (VId nm :-: ty) -> do
    env <- gets regmap
    return $ OpVar (VId (show (env Map.! nm)) :-: ty)
replaceTerm term = case term of
  TRet op -> TRet <$> replaceOperand op
  TBr op b1 b2 -> do
    repOp <- replaceOperand op
    return $! TBr repOp b1 b2
  TJmp _ -> return term

getLoc :: VId -> M VId
getLoc (VId nm) = do
  x <- gets (fromJust . Map.lookup nm . regmap)
  return $! VId $! show x
newReg :: (VId, Type) -> M ()
newReg (VId vname, ty) = do
  RegEnv rmap gr fr st <- get
  let vac = case ty of
       TFloat -> complement fr
       _      -> complement gr
  if vac == 0 then do
    let sst = Stack st
    modify $ \s -> s { regmap = Map.insert vname sst rmap, stack = st + 1 }
  else do
    let min = vac .&. (- vac)
    let num = popCount (min - 1)
    let reg = case ty of { TFloat -> FReg num; _ -> Reg num }
    allocReg reg (VId vname)
allocReg :: Loc -> VId -> M ()
allocReg reg (VId vname) = do
  RegEnv rmap gr fr _ <- get
  case reg of
    Reg regnum ->
      let pos = 1 `shiftL` regnum in
      modify $ \s -> s { regmap = Map.insert vname reg rmap, gregs = gr .|. pos }
    FReg regnum ->
      let pos = 1 `shiftL` regnum in
      modify $ \s -> s { regmap = Map.insert vname reg rmap, fregs = fr .|. pos }
    Stack _ ->
      modify $ \s -> s { regmap = Map.insert vname reg rmap }
freeReg :: Loc -> M ()
freeReg _v = undefined
