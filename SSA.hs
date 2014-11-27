{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module SSA where


import Closure
import Type
import Typing (TypeEnv)
import Id
import Syntax


import Control.Applicative ((<$>))
import Control.Monad.Reader
import Data.Int
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe

import Control.Monad.Identity
import Control.Monad.State

type M = CounterT (Reader TypeEnv)


data SSAFundef = SSAFundef
  { name :: !(Typed LId)
  , args :: ![Typed VId]
  , formalFV :: ![Typed VId]
  , blocks :: ![Block]
  } deriving (Eq)

instance Show SSAFundef where
  show (SSAFundef (LId n :-: funTy) args formFV blks) = 
    "def @" ++ n ++ "(" ++ List.intercalate ", " (map (\(_ :-: ty) -> show ty) args) ++ ") : " ++ show retTy ++ "\n" ++ show blks
   where
    TFun _ retTy = funTy

data Block = Block !BlockID ![Inst] !Term
  deriving (Eq)

instance Show Block where
  show (Block bid insts term) = 
    bid ++ ":\n" ++
    concatMap (\x -> "  " ++ show x ++ "\n") insts ++
    "  " ++ show term ++ "\n"

type BlockID = String

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
  | SPhi ![(BlockID, Operand)]
  deriving (Eq, Show)
data Term =
  TRet !Operand
  | TBr !Operand !BlockID !BlockID
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
ssaTransFun (CFundef (VId lid, ty) args formFV expr) =
  let typedArg = map (\(x,t) -> x :-: t) args in
  let typedFV  = map (\(x,t) -> x :-: t) formFV in
  SSAFundef (LId lid :-: ty) typedArg typedFV <$> (fmap getBlocks $ execStateT (ssaTransExpr expr) (emptyState (typedArg ++ typedFV)))


ssaTransExpr :: ClosExp -> StateT CgenState M ()
getOperand :: ClosExp -> StateT CgenState M Operand

ssaTransExpr exprty@(expr :-: ty) = do
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
    ty <- lookupTypeInfo x
    fresh <- freshVar TInt
    addInst $ Inst (Just fresh) $ SCmpBin operator (OpVar (x :-: ty)) (OpVar (y :-: ty))
    thenBlk <- newBlock "then"
    elseBlk <- newBlock "else"
    contBlk <- newBlock "cont"
    addTerm $ TBr (OpVar (fresh :-: TInt)) thenBlk elseBlk
    setBlock thenBlk
    oth <- getOperand e1
    thenEnd <- getBlock
    addTerm $ TBr (ci32 1) contBlk contBlk
    setBlock elseBlk
    oel <- getOperand e2
    elseEnd <- getBlock
    addTerm $ TBr (ci32 1) contBlk contBlk
    setBlock contBlk
    let retTy = getType oth
    retFresh <- freshVar retTy
    addInst $ Inst (Just retFresh) $ SPhi [(thenEnd, oth), (elseEnd, oel)]
    return (OpVar (retFresh :-: retTy))
  CLet vid@(VId i) ty e1 e2 -> do
    res <- getOperand e1
    addInst (Inst (Just vid) (SId res))
    addTypeInfo vid ty 
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
  CMakeCls clsVid@(VId clsName) ty clos expr -> do
    {- TODO No initialization is performed. -}
    addTypeInfo clsVid ty
    getOperand expr    
  CAppCls fun@(VId funname) args -> do
    funType@(TFun argType retType) <- lookupTypeInfo fun
    fresh <- freshVar retType
    let clsType = TArray TUnit
    let clsId = OpVar (fun :-: clsType)
    argsId <- forM args $ \vx -> do
      ty <- lookupTypeInfo vx
      return (OpVar (vx :-: ty))
    addInst $ Inst (Just fresh) $ SCall (LId funname :-: TFun (clsType : argType) retType)  (clsId : argsId)
    return (OpVar (fresh :-: retType))
  CAppDir fun@(LId funname) args -> do
    funType@(TFun _ retType) <- do
      if funname == "min_caml_create_array" then
         let TArray elemTy = ty in
           return $ TFun [TInt,elemTy] ty
      else
        asks (fromMaybe (error $ "type of " ++ show fun ++ " is not found") . Map.lookup (Id funname))
    fresh <- freshVar retType
    argsId <- forM args $ \vx -> do
      ty <- lookupTypeInfo vx
      return (OpVar (vx :-: ty))
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
  CLetTuple elems tuple expr -> do
    let size = 4 * length elems {- TODO This code assumes that sizeof int, float, ptr are all 4. -}
    forM_ [0 .. length elems - 1] $ \i -> do
      let (argid, argTy) = elems !! i
      addInst $ Inst (Just argid) $ SCall (LId "array_get" :-: (TFun [TArray TUnit, TInt] argTy)) [OpVar (tuple :-: TArray TUnit), ci32 (4 * i)]
    getOperand expr
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
  CExtArray (LId x) -> 
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
  addInst $ Inst Nothing $ SCall (LId "array_put" :-: (TFun [getType aryOp, TInt, getType elemOp] TUnit))
    [aryOp, idxOp, elemOp]
arrayGet :: Operand -> Operand -> StateT CgenState M Operand
arrayGet aryOp idxOp = do
  let aryTy@(TArray elemTy) = getType aryOp
  fresh <- freshVar　elemTy
  addInst $ Inst (Just fresh) $ SCall (LId "array_get" :-: (TFun [aryTy, TInt] elemTy))
    [aryOp, idxOp]
  return (OpVar (fresh :-: elemTy))

emptyState :: [Typed VId] -> CgenState
emptyState vids = CgenState 0 "entry" (Map.fromList [("entry", Block "entry" [] (TRet (OpConst UnitConst)))])
  (Map.fromList $ map (\(x :-: t) -> (x, t)) vids)

addInst :: MonadState CgenState m => Inst -> m ()
addInst inst = do
  nm <- gets current
  Block blkId insts term <- gets (fromJust . Map.lookup nm . accBlocks)
  modify (\s -> s { accBlocks = Map.insert nm (Block blkId (insts ++ [inst]) term) (accBlocks s) })


addTerm :: Term -> StateT CgenState M ()
addTerm term = do
  nm <- gets current
  Block blkId insts _ <- gets (fromJust . Map.lookup nm . accBlocks)
  modify (\s -> s { accBlocks = Map.insert nm (Block blkId insts term) (accBlocks s) })

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
  let newBlk = Block newBlockName [] (TRet (OpConst UnitConst))
  modify $ \s -> s { blockIdx = idx + 1, accBlocks = Map.insert newBlockName newBlk (accBlocks s)　}
  return newBlockName

setBlock :: BlockID -> StateT CgenState M ()
setBlock blkID =
  modify $ \s -> s { current = blkID }

getBlock :: StateT CgenState M BlockID
getBlock = gets current


getBlocks :: CgenState -> [Block]
getBlocks st = map snd (Map.toList (accBlocks st))



getType :: Operand -> Type
getType (OpVar (_ :-: ty)) = ty
getType (OpConst c) = case c of
  IntConst _ -> TInt
  FloatConst _ -> TFloat
  UnitConst -> TUnit

