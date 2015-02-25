{- One of the most naive register allocation -}
module RegAlloc where

import Control.Applicative
import Control.Monad.State
import Data.Bits
import Data.Function (on)
import Data.Maybe
import qualified Data.List as List
import qualified Data.Set as Set
import Data.Word
import qualified Data.Map as Map
import SSA hiding (M)
import Id
import Type
import SSALiveness
import Interfere
import SSASimpl (replaceInst) {- for replacing instructions -}

import Debug.Trace


data RegEnv = RegEnv { regmap :: !(Map.Map String Loc), stackId :: Int　}
emptyEnv :: RegEnv
emptyEnv = RegEnv Map.empty 0
type M = State RegEnv
data Loc = Reg !Int | FReg !Int | Stack !Int deriving (Eq)

instance Show Loc where
  show (Reg v) = '$' : show v
  show (FReg v) = "$f" ++ show v
  show (Stack i) = "st" ++ show i

regAlloc :: [SSAFundef] -> [SSAFundef]


regAlloc = map regAllocFundef where
  regAllocFundef fundef = evalState (rab fundef) emptyEnv

gregs :: [Loc]
fregs :: [Loc]

gregs = [ Reg i | i <- [0 .. 24] ++ [26]]
fregs = [ FReg i | i <- [0 .. 30]]



rab :: SSAFundef -> M SSAFundef
rab fundef@(SSAFundef nty params formFV blks) = do
  let infGr = accLiveInfo (analyzeLiveness fundef)
  let gcol = [0 .. length gregs - 1]
  let fcol = [length gregs .. length gregs + length fregs - 1]
  let argColoring xs gcol fcol = case xs of
       [] -> []
       x : rest -> case x of
         v :-: TFloat -> (v, head fcol) : argColoring rest gcol (tail fcol)
         v :-: _      -> (v, head gcol) : argColoring rest (tail gcol) fcol
  let argCol = Map.fromList $ argColoring params gcol fcol
  let vars = concatMap RegAlloc.varsBlock blks
  let colset = Map.fromList [if ty /= TFloat then (i, gcol) else (i, fcol) | i :-: ty <- vars]
  let coloring = tryColoring infGr colset argCol
  let regGet vid regmap = case Map.lookup vid regmap of { Nothing -> Reg 31; Just k -> (gregs ++ fregs) !! k; }
  case coloring of
    Left remaining -> do
      let spilled = head $ List.sortBy (compare `on` (\x -> numUseFundef x fundef)) (Set.elems remaining) -- spills the vairable least frequently used
      newfundef <- spillVar (lookupTyped spilled vars) fundef
      rab newfundef
    Right regmap -> do
      forM_ params $ \(vid :-: ty) ->
        allocReg (regGet vid regmap) vid {- TODO This code assumes that all variables are of the same type (int) -}
      forM_ vars $ \(vid :-: ty) ->
        allocReg (regGet vid regmap) vid
      newBlks <- mapM replace blks
      newParams <- forM params $ \(vid :-: ty) -> do
        l <- getLoc vid
        return $! l :-: ty
      return $! SSAFundef nty newParams formFV newBlks

varsBlock :: Block -> [Typed VId]
varsBlock (Block _ (Phi vars cols) insts _) = g ++ concatMap f insts where
  f (Inst Nothing _) = []
  f (Inst (Just v) op) = [v :-: typeOfOp op]
  g = [ (vars !! i) :-: getType ((cols Map.! head (Map.keys cols)) !! i) | i <- [0 .. length vars - 1]]

lookupTyped :: Eq a => a -> [Typed a] -> Typed a
lookupTyped x [] = error $ "type was not found"
lookupTyped x ((y :-: ty) : ys) | x == y = y :-: ty
lookupTyped x (_ : ys) = lookupTyped x ys

replace :: Block -> M Block
replaceInst :: Inst -> M Inst
replaceOp :: Op -> M Op
replaceOperand :: Operand -> M Operand
replaceTerm :: Term -> M Term

replace (Block blkId phi insts term) = Block blkId <$> return phi <*> mapM RegAlloc.replaceInst insts <*> replaceTerm term

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
  RegEnv rmap _ <- get
　　modify $ \s -> s { regmap = Map.insert vname reg rmap }

spillVar :: Typed VId -> SSAFundef -> M SSAFundef
spillVar vid fundef@(SSAFundef nty params formFV blks) = 
  trace ("spilling variable " ++ show vid ++ "...\n") $
  if elem vid params then
    error $ "attempt to spill parameter:" ++ show vid
  else do
    fid <- freshId
    let x = SSAFundef nty params formFV $ runCounter $ mapM (spillVarSub vid fid) blks
    return x

spillVarSub :: Typed VId -> Int -> Block -> Counter Block
spillVarSub vidty@(vid@(VId nm) :-: ty) loc (Block blk phi insts term) = do
  newinsts <- fmap concat $ forM insts $ \inst@(Inst dest op) ->
    if dest == Just vid then
      return [inst, storeVar vidty loc]
    else if elem vid (fvOp op) then do
      freshId <- genVId (nm ++ "." ++ blk)
      trace ("spilling " ++ show vid ++ "->" ++ show freshId ++ "...") $ return ()
      return [loadVar (freshId :-: ty) loc, SSASimpl.replaceInst (Map.singleton vid freshId) inst]
    else
      return [inst]
  return $ Block blk phi newinsts term
storeVar :: Typed VId -> Int -> Inst
storeVar (vid :-: ty) loc = Inst Nothing $ SCall (LId "@store" :-: TFun [ty, TInt] TUnit) [OpVar (vid :-: ty), ci32 loc]

loadVar :: Typed VId -> Int -> Inst
loadVar (vid :-: ty) loc = Inst (Just vid) $ SCall (LId "@load" :-: TFun [TInt] ty) [ci32 loc]

freshId :: M Int
freshId = do
  i <- gets stackId
  modify $ \s -> s { stackId = i + 1 }
  return i

numUseFundef :: VId -> SSAFundef -> Int
numUseFundef vid (SSAFundef { blocks = blk }) = sum $ map (numUseBlock vid) blk


-- | Rough heuristic function. This function does not count occurrence of vid in phi nodes.
numUseBlock :: VId -> Block -> Int
numUseBlock vid (Block _ _phi insts term) = sum (map f insts) + g term where
  f (Inst _ op) = if elem vid (fvOp op) then 1 else 0
  g t = if elem vid (fvTerm t) then 1 else 0

