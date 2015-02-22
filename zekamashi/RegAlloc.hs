{- One of the most naive register allocation -}
module RegAlloc where

import Control.Applicative
import Control.Monad.State
import Data.Bits
import Data.Maybe
import Data.Word
import qualified Data.Map as Map
import SSA hiding (M)
import Id
import Type
import SSALiveness
import Interfere

import Debug.Trace


data RegEnv = RegEnv { regmap :: !(Map.Map String Loc)　}
emptyEnv :: RegEnv
emptyEnv = RegEnv Map.empty
type M = State RegEnv
data Loc = Reg !Int | FReg !Int | Stack !Int deriving (Eq)

instance Show Loc where
  show (Reg v) = '$' : show v
  show (FReg v) = "$f" ++ show v
  show (Stack i) = "st" ++ show i

regAlloc :: [SSAFundef] -> [SSAFundef]


regAlloc = map regAllocFundef where
  regAllocFundef fundef = evalState (rab fundef) emptyEnv


rab :: SSAFundef -> M SSAFundef
rab fundef@(SSAFundef nty params formFV blks) = do
  let infGr = accLiveInfo (analyzeLiveness fundef)
  traceShow infGr $ return ()
  let coloring = tryColoring infGr 24
  case coloring of
    Nothing -> fail "Failed to allocate register (spilling is not yet supported)."
    Just regmap -> do
      let vars = concatMap RegAlloc.varsBlock blks
      forM_ params $ \(vid :-: ty) ->
        allocReg (Reg (regmap Map.! vid)) vid {- TODO This code assumes that all variables are of the same type (int) -}
      forM_ vars $ \(vid, ty) ->
        allocReg (Reg (regmap Map.! vid)) vid
      newBlks <- mapM replace blks
      newParams <- forM params $ \(vid :-: ty) -> do
        l <- getLoc vid
        return $! l :-: ty
      return $! SSAFundef nty newParams formFV newBlks

varsBlock :: Block -> [(VId, Type)]
varsBlock (Block _ _ insts _) = concatMap f insts where
  f (Inst Nothing _) = []
  f (Inst (Just v) op) = [(v, typeOfOp op)]


replace :: Block -> M Block
replaceInst :: Inst -> M Inst
replaceOp :: Op -> M Op
replaceOperand :: Operand -> M Operand
replaceTerm :: Term -> M Term

replace (Block blkId phi insts term) = Block blkId <$> return phi <*> mapM replaceInst insts <*> replaceTerm term

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
allocReg :: Loc -> VId -> M ()
allocReg reg (VId vname) = do
  RegEnv rmap <- get
　　modify $ \s -> s { regmap = Map.insert vname reg rmap　}

