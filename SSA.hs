{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module SSA where


import Closure hiding (args)
import Type
import Typing (TypeEnv)
import Id hiding (fresh)
import Syntax hiding (args)


import Control.Applicative ((<$>))
import Control.Monad (forM, forM_)
import Control.Monad.Reader (Reader, asks)
import Control.Monad.State (MonadState, StateT, execStateT, gets, modify)
import Control.Monad.Trans (lift)
import Data.Int (Int32)
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust, fromMaybe, maybeToList)


type M = CounterT (Reader TypeEnv)


data SSAFundef = SSAFundef
  { name :: !(Typed LId)
  , args :: ![Typed VId]
  , formalFV :: ![Typed VId]
  , blocks :: ![Block]
  } deriving (Eq)

instance Show SSAFundef where
  show (SSAFundef (LId n :-: funTy) args_ _formFV blks) = 
    "def @" ++ n ++ "(" ++ List.intercalate ", " (map showArg args_) ++ ") : " ++ show retTy ++ "\n" ++ show blks
   where
    TFun _ retTy = funTy
    showArg (VId nm :-: ty) = "$" ++ nm ++ " : " ++ show ty
entryBlockName :: BlockID
entryBlockName = "entry"

data Block = Block !BlockID !Phi ![Inst] !Term
  deriving (Eq)

instance Show Block where
  show (Block bid phi insts term) = 
    bid ++ ":\n" ++
    show phi ++
    concatMap (\x -> "  " ++ show x ++ "\n") insts ++
    "  " ++ show term ++ "\n"

type BlockID = String

data Phi = Phi { phivars :: ![VId],  columns :: !(Map BlockID [Operand]) }
  deriving (Eq)

instance Show Phi where
  show (Phi _vars cols) | Map.null cols = ""
  show (Phi vars cols) = show vars ++ " := " ++ show cols ++ "\n"


data Inst = Inst !(Maybe VId) !Op
  deriving (Eq)

instance Show Inst where
  show (Inst Nothing op) = show op
  show (Inst (Just (VId v)) op) = "$" ++ v ++ " := " ++ show op


data Op = 
  SId !Operand
  | SArithBin !ArithBinOp !Operand !Operand
  | SFloatBin !FloatBinOp !Operand !Operand
  | SCmpBin !CmpOp !Operand !Operand
  | SNeg !Operand
  | SFNeg !Operand
  | SCall !(Typed LId) ![Operand]
  deriving (Eq, Show)
data Term =
  TRet !Operand
  | TBr !Operand !BlockID !BlockID
  | TJmp !BlockID
  deriving (Eq, Show)
data Operand = OpVar !(Typed VId) | OpConst !Const
  deriving (Eq)

instance Show Operand where
  show (OpVar (VId v :-: _)) = "$" ++ v
  show (OpConst c) = show c


data Const = 
  IntConst !Int32
  | FloatConst !Double
  | UnitConst
  deriving (Eq)

instance Show Const where
  show (IntConst x) = show x
  show (FloatConst x) = show x
  show UnitConst = "()"

ssaTrans :: [CFundef] -> ClosExp -> M [SSAFundef]
ssaTrans defs (expr :-: ty) = do
  defsTrans <- mapM ssaTransFun defs
  mainTrans <- ssaTransFun (CFundef ("main", TFun [] ty) [] [] (expr :-: ty))
  return (defsTrans ++ [mainTrans])

ssaTransFun :: CFundef -> M SSAFundef
ssaTransFun (CFundef (VId lid, ty) args_ formFV expr) =
  let toTyped (x, t) = x :-: t in
  let typedArg = map toTyped args_ in
  let typedFV  = map toTyped formFV in
  SSAFundef (LId lid :-: ty) typedArg typedFV <$> (getBlocks <$> execStateT (ssaTransExpr expr) (emptyState (typedArg ++ typedFV)))


ssaTransExpr :: ClosExp -> StateT CgenState M ()
getOperand :: ClosExp -> StateT CgenState M Operand

ssaTransExpr exprty = do
  op <- getOperand exprty
  addTerm (TRet op)
getOperand (expr :-: ty) = case expr of
  CUnit -> return (OpConst UnitConst)
  CInt v -> return  (ci32 v)
  CFloat v -> return (OpConst (FloatConst (fromRational (toRational v))))
  CNeg vid  -> do
    fresh <- freshVar TInt
    addInst (Inst (Just fresh) (SNeg (OpVar (vid :-: TInt))))
    return (OpVar (fresh :-: TInt))
  CFNeg vid  -> do
    fresh <- freshVar TFloat
    addInst (Inst (Just fresh) (SFNeg (OpVar (vid :-: TFloat))))
    return (OpVar (fresh :-: TFloat))
  CIf operator x y e1 e2 -> do
    oty <- lookupTypeInfo x
    fresh <- freshVar TInt
    addInst $ Inst (Just fresh) $ SCmpBin operator (OpVar (x :-: oty)) (OpVar (y :-: oty))
    curBlk  <- getBlock
    thenBlk <- newBlock "then"
    elseBlk <- newBlock "else"
    contBlk <- newBlock "cont"
    addTerm $ TBr (OpVar (fresh :-: TInt)) thenBlk elseBlk
    setBlock thenBlk
    setPhi [] (Map.singleton curBlk [])
    oth <- getOperand e1
    thenEnd <- getBlock
    addTerm $ TJmp contBlk
    setBlock elseBlk
    setPhi [] (Map.singleton curBlk [])
    oel <- getOperand e2
    elseEnd <- getBlock
    addTerm $ TJmp contBlk
    setBlock contBlk
    let retTy = getType oth
    retFresh <- freshVar retTy
    setPhi [retFresh] (Map.fromList [(thenEnd, [oth]), (elseEnd, [oel])])
    return (OpVar (retFresh :-: retTy))
  CLet vid vty e1 e2 -> do
    res <- getOperand e1
    addInst (Inst (Just vid) (SId res))
    addTypeInfo vid vty 
    getOperand e2
  CVar vid -> return (OpVar (vid :-: ty))
  CArithBin op x y -> do
    fresh <- freshVar　TInt
    addInst $ Inst (Just fresh) $ SArithBin op (OpVar (x :-: TInt)) (OpVar (y :-: TInt))
    return (OpVar (fresh :-: TInt))
  CFloatBin op x y -> do
    fresh <- freshVar TFloat
    addInst $ Inst (Just fresh) $ SFloatBin op (OpVar (x :-: TFloat)) (OpVar (y :-: TFloat))
    return (OpVar (fresh :-: TFloat))
  CMakeCls clsVid cty _ e -> do
    {- TODO No initialization is performed. -}
    addTypeInfo clsVid cty
    getOperand e
  CAppCls fun@(VId funname) args_ -> do
    TFun argType retType <- lookupTypeInfo fun
    fresh <- freshVar retType
    let clsType = TArray TUnit
    let clsId = OpVar (fun :-: clsType)
    argsId <- forM args_ $ \vx -> do
      vty <- lookupTypeInfo vx
      return (OpVar (vx :-: vty))
    addInst $ Inst (Just fresh) $ SCall (LId funname :-: TFun (clsType : argType) retType)  (clsId : argsId)
    return (OpVar (fresh :-: retType))
  CAppDir fun@(LId funname) args_ -> do
    funType@(TFun _ retType) <-
      if funname == "min_caml_create_array" then
         let TArray elemTy = ty in
           return $ TFun [TInt,elemTy] ty
      else
        asks (fromMaybe (error $ "type of " ++ show fun ++ " is not found") . Map.lookup (Id funname))
    fresh <- freshVar retType
    argsId <- forM args_ $ \vx -> do
      vty <- lookupTypeInfo vx
      return (OpVar (vx :-: vty))
    addInst $ Inst (Just fresh) $ SCall (fun :-: funType) argsId
    return (OpVar (fresh :-: retType))
  CTuple elems -> do
    let TTuple elemTy = ty
    let retType = TArray TUnit
    let funType = TFun [TInt] retType
    let size = 4 * length elems {- TODO This code assumes that sizeof int, float, ptr are all 4. -}
    fresh <- freshVar retType
    addInst $ Inst (Just fresh) $ SCall (LId "malloc" :-: funType) [ci32 size]
    forM_ [0 .. length elems - 1] $ \i -> do
      let (argid, argty) = zip elems elemTy !! i
      arrayPut (OpVar (fresh :-: retType)) (ci32 (4 * i)) (OpVar (argid :-: argty))
    return (OpVar (fresh :-: retType))
  CLetTuple elems tuple e -> do
    forM_ [0 .. length elems - 1] $ \i -> do
      let (argid, argTy) = elems !! i
      addInst $ Inst (Just argid) $ SCall (LId "array_get" :-: TFun [TArray TUnit, TInt] argTy) [OpVar (tuple :-: TArray TUnit), ci32 (4 * i)] {- TODO This code assumes that sizeof int, float, ptr are all 4. -}
    getOperand e
  CGet x y -> do
    xty <- lookupTypeInfo x
    yty <- lookupTypeInfo y
    arrayGet (OpVar (x :-: xty)) (OpVar (y :-: yty))
  CPut x y z -> do
    xty <- lookupTypeInfo x
    yty <- lookupTypeInfo y
    zty <- lookupTypeInfo z
    arrayPut (OpVar (x :-: xty)) (OpVar (y :-: yty)) (OpVar (z :-: zty))
    return (OpConst UnitConst)
  CExtArray (LId _) -> 
    error "Undefined instruction: extarray"
data CgenState = CgenState
  { blockIdx :: Int
  , current :: String
  , accBlocks :: Map String Block
  , typeEnv :: Map VId Type
  } deriving (Show)

ci32 :: Integral i => i -> Operand
ci32 i = OpConst (IntConst (fromIntegral i))

arrayPut :: MonadState CgenState m => Operand -> Operand -> Operand -> m ()
arrayPut aryOp idxOp elemOp =
  addInst $ Inst Nothing $ SCall (LId "array_put" :-: TFun [getType aryOp, TInt, getType elemOp] TUnit)
    [aryOp, idxOp, elemOp]
arrayGet :: Operand -> Operand -> StateT CgenState M Operand
arrayGet aryOp idxOp = do
  let aryTy@(TArray elemTy) = getType aryOp
  fresh <- freshVar　elemTy
  addInst $ Inst (Just fresh) $ SCall (LId "array_get" :-: TFun [aryTy, TInt] elemTy)
    [aryOp, idxOp]
  return (OpVar (fresh :-: elemTy))

emptyPhi :: Phi
emptyPhi = Phi [] Map.empty

emptyState :: [Typed VId] -> CgenState
emptyState vids = CgenState 0 entryBlockName (Map.fromList [(entryBlockName, Block entryBlockName (Phi [] Map.empty) [] (TRet (OpConst UnitConst)))])
  (Map.fromList $ map (\(x :-: t) -> (x, t)) vids)

setPhi :: MonadState CgenState m => [VId] -> Map BlockID [Operand] -> m ()
setPhi vs ops = do
  nm <- gets current
  Block blkId _phi insts term <- gets (fromJust . Map.lookup nm . accBlocks)
  modify (\s -> s { accBlocks = Map.insert nm (Block blkId (Phi vs ops) insts term) (accBlocks s) })
addInst :: MonadState CgenState m => Inst -> m ()
addInst inst = do
  nm <- gets current
  Block blkId phi insts term <- gets (fromJust . Map.lookup nm . accBlocks)
  modify (\s -> s { accBlocks = Map.insert nm (Block blkId phi (insts ++ [inst]) term) (accBlocks s) })


addTerm :: Term -> StateT CgenState M ()
addTerm term = do
  nm <- gets current
  Block blkId phi insts _ <- gets (fromJust . Map.lookup nm . accBlocks)
  modify (\s -> s { accBlocks = Map.insert nm (Block blkId phi insts term) (accBlocks s) })

freshVar :: Type -> StateT CgenState M VId
freshVar ty = do
  Id str <- lift $ genId "SSA"
  addTypeInfo (VId str) ty
  return (VId str)


addTypeInfo :: VId -> Type -> StateT CgenState M ()
addTypeInfo vid ty = modify (\s ->  s { typeEnv = Map.insert vid ty $ typeEnv s })

lookupTypeInfo :: VId -> StateT CgenState M Type
lookupTypeInfo vid = gets (fromMaybe (error $ "type of " ++ show vid ++ " is not found") . Map.lookup vid . typeEnv)

newBlock :: String -> StateT CgenState M BlockID
newBlock str = do
  idx <- gets blockIdx
  let newBlockName = str ++ "." ++ show idx
  let newBlk = Block newBlockName emptyPhi [] (TRet (OpConst UnitConst))
  modify $ \s -> s { blockIdx = idx + 1, accBlocks = Map.insert newBlockName newBlk (accBlocks s)　}
  return newBlockName

setBlock :: BlockID -> StateT CgenState M ()
setBlock blkID =
  modify $ \s -> s { current = blkID }

getBlock :: StateT CgenState M BlockID
getBlock = gets current


getBlocks :: CgenState -> [Block]
getBlocks st = map snd (Map.toList (accBlocks st))

-- Utility functions

getType :: Operand -> Type
getType (OpVar (_ :-: ty)) = ty
getType (OpConst c) = case c of
  IntConst _ -> TInt
  FloatConst _ -> TFloat
  UnitConst -> TUnit

typeOfOp :: Op -> Type
typeOfOp e = case e of
  SId o -> getType o
  SArithBin {} -> TInt
  SFloatBin {} -> TFloat
  SCmpBin _ o1 _ -> getType o1
  SNeg {} -> TInt
  SFNeg {} -> TFloat
  SCall (_ :-: TFun _ retTy) _ -> retTy
  _ -> error $ "not a valid Op: " ++ show e

mapEndoBlock :: (Block -> Block) -> SSAFundef -> SSAFundef
mapEndoBlock f fundef@(SSAFundef {blocks = blks} ) =
  fundef { blocks = map f blks }

mapEndoBlocks :: ([Block] -> [Block]) -> SSAFundef -> SSAFundef
mapEndoBlocks f fundef@(SSAFundef {blocks = blks} ) =
  fundef { blocks = f blks }

minFix :: Eq q => (q -> q) -> q -> q
minFix f initVal = let e = f initVal in
  if e == initVal then initVal else minFix f e

varsFundef :: SSAFundef -> [VId]
varsFundef (SSAFundef { blocks = blks }) = List.concatMap varsBlock blks
varsBlock :: Block -> [VId]
varsBlock (Block _ (Phi vars _) insts _term) = vars ++ List.concatMap varInst insts

varInst :: Inst -> [VId]
varInst (Inst x _) = maybeToList x

fvBlock :: Block -> [VId]
fvBlock (Block _blkId phi insts term) = fvPhi phi ++ concatMap (\(Inst _ op) -> fvOp op) insts ++ fvTerm term

fvPhi :: Phi -> [VId]
fvPhi (Phi _ cols) = concatMap fvOperand $ concat (Map.elems cols)

fvOp :: Op -> [VId]
fvOp op = case op of
  SId x -> fvOperand x
  SNeg x -> fvOperand x
  SFNeg x -> fvOperand x
  SArithBin _ x y -> fvOperand x ++ fvOperand y
  SCmpBin _ x y -> fvOperand x ++ fvOperand y
  SFloatBin _ x y -> fvOperand x ++ fvOperand y
  SCall _ args_ -> concatMap fvOperand args_

fvTerm :: Term -> [VId]
fvTerm (TRet op) = fvOperand op
fvTerm (TBr op _ _) = fvOperand op
fvTerm (TJmp {}) = []

fvOperand :: Operand -> [VId]
fvOperand (OpVar (vid :-: _)) = [vid]
fvOperand _ = []


