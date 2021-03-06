{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Emit where

import Closure
import Id (VId(..), LId(..))
import qualified Id
import qualified Syntax
import Type
import qualified Type
import Control.Monad ((>=>), forM_, join, when, liftM, ap)
import Control.Monad.State (MonadState, StateT, execStateT, gets, modify, runStateT)
import Control.Applicative (Applicative, (<$>), (<*>))
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT, asks)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Except (ExceptT, MonadError, runExceptT, throwError)
import Data.Int (Int32, Int64)
import qualified Data.Map as Map
import Data.Word (Word)
import Data.List (sortBy)
import qualified Data.List as List
import Data.Maybe (fromJust)
import Data.Function (on)
import qualified Data.Map as Map

import LLVM.General.AST (Definition(..), FloatingPointFormat(..), Instruction(..), Name(..), Named(..), Operand(..), Terminator(..), Type(..), defaultModule, moduleDefinitions, moduleName)
import LLVM.General.AST.AddrSpace
import LLVM.General.AST.Global (BasicBlock(BasicBlock), Parameter(Parameter), basicBlocks, functionDefaults, name, parameters, returnType)
import qualified LLVM.General.AST.Global as Global
import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Attribute as A
import qualified LLVM.General.AST.CallingConvention as CC
import qualified LLVM.General.AST.Constant as C
import qualified LLVM.General.AST.Float as F
import qualified LLVM.General.AST.IntegerPredicate as IP
import qualified LLVM.General.AST.Type as T
import LLVM.General.Context (Context, withContext)
import LLVM.General.Module
import LLVM.General.PassManager (PassSetSpec, defaultCuratedPassSetSpec, optLevel, runPassManager, withPassManager)

newtype LLVM a = LLVM { unLLVM :: StateT AST.Module (Either String) a }
  deriving (Functor, Applicative, Monad, MonadState AST.Module, MonadError String)

runLLVM :: AST.Module -> LLVM a -> Either String AST.Module
runLLVM astmod llvm = execStateT (unLLVM llvm) astmod

emptyModule :: String -> AST.Module
emptyModule label = defaultModule { moduleName = label }

addDefn :: Definition -> LLVM ()
addDefn d = do
  defs <- gets moduleDefinitions
  modify $ \s -> s { moduleDefinitions = defs ++ [d] }

define ::  AST.Type -> String -> [(AST.Type, Name)] -> [BasicBlock] -> LLVM ()
define retty label argtys body = addDefn $
  GlobalDefinition $ functionDefaults {
    Global.name        = Name label
  , parameters  = ([Parameter ty nm [] | (ty, nm) <- argtys], False)
  , returnType  = retty
  , basicBlocks = body
  }

external ::  AST.Type -> String -> [(AST.Type, Name)] -> LLVM ()
external retty label argtys = addDefn $
  GlobalDefinition $ functionDefaults {
    Global.name        = Name label
  , parameters  = ([Parameter ty nm [] | (ty, nm) <- argtys], False)
  , returnType  = retty
  , basicBlocks = []
  }

---------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- IEEE 754 double
double :: AST.Type
double = FloatingPointType 64 IEEE

-- IEEE 754 float
float :: AST.Type
float = FloatingPointType 32 IEEE

-- 1-bit integer (boolean)
int1 :: AST.Type
int1 = IntegerType 1

-- 8-bit integer (byte)
int8 :: AST.Type
int8 = IntegerType 8

-- 32-bit integer
int32 :: AST.Type
int32 = IntegerType 32

-- 64-bit integer
int64 :: AST.Type
int64 = IntegerType 64

-------------------------------------------------------------------------------
-- Names
-------------------------------------------------------------------------------

type Names = Map.Map String Int

uniqueName :: String -> Names -> (String, Names)
uniqueName nm ns =
  case Map.lookup nm ns of
    Nothing -> (nm,  Map.insert nm 1 ns)
    Just ix -> (nm ++ show ix, Map.insert nm (ix+1) ns)

-------------------------------------------------------------------------------
-- Codegen State
-------------------------------------------------------------------------------

type SymbolTable = [(String, Operand)]

data CodegenState
  = CodegenState {
    currentBlock :: Name                     -- Name of the active block to append to
  , blocks       :: Map.Map Name BlockState  -- Blocks for function
  , symtab       :: SymbolTable              -- Function scope symbol table
  , blockCount   :: Int                      -- Count of basic blocks
  , count        :: Word                     -- Count of unnamed instructions
  , names        :: Names                    -- Name Supply
  , loopExits    :: [Name]                   -- loop exit info
  } deriving Show

data BlockState
  = BlockState {
    idx   :: Int                            -- Block index
  , stack :: [Named Instruction]            -- Stack of instructions
  , term  :: Maybe (Named Terminator)       -- Block terminator
  } deriving Show

-------------------------------------------------------------------------------
-- Codegen Operations
-------------------------------------------------------------------------------

newtype Codegen a = Codegen { unCodegen :: ReaderT (Map.Map String CFundef) (StateT CodegenState (Either String)) a }
  deriving (Functor, Applicative, Monad, MonadState CodegenState, MonadReader (Map.Map String CFundef), MonadError String )

sortBlocks :: [(Name, BlockState)] -> [(Name, BlockState)]
sortBlocks = sortBy (compare `on` (idx . snd))

createBlocks :: CodegenState -> [BasicBlock]
createBlocks m = map makeBlock $ sortBlocks $ Map.toList (blocks m)

makeBlock :: (Name, BlockState) -> BasicBlock
makeBlock (l, BlockState _ s t) = BasicBlock l s (maketerm t)
  where
    maketerm (Just x) = x
    maketerm Nothing = error $ "Block has no terminator: " ++ show l

entryBlockName :: String
entryBlockName = "entry"

emptyBlock :: Int -> BlockState
emptyBlock i = BlockState i [] Nothing

emptyCodegen :: CodegenState
emptyCodegen = CodegenState (Name entryBlockName) Map.empty [] 1 0 Map.empty []

execCodegen :: [CFundef] -> Codegen a -> Either String CodegenState
execCodegen fundefs m = fmap snd (runCodegen fundefs m)

runCodegen :: [CFundef] -> Codegen a -> Either String (a, CodegenState)
runCodegen fundefs m = runStateT (runReaderT (unCodegen m) mp) emptyCodegen
  where
   mp = Map.fromList (map (\cfd@(CFundef (VId n, _) _ _ _) -> (n, cfd)) fundefs)

fresh :: Codegen Word
fresh = do
  i <- gets count
  modify $ \s -> s { count = 1 + i }
  return $ i + 1

instr :: Instruction -> Codegen Operand
instr ins = do
  n <- fresh
  let ref = UnName n
  blk <- current
  let i = stack blk
  modifyBlock (blk { stack = i ++ [ref := ins] } )
  return $ local ref

addInstr :: Name -> Instruction -> Codegen ()
addInstr ref ins = do
  blk <- current
  let i = stack blk
  modifyBlock (blk { stack = i ++ [ref := ins] } )


terminator :: Named Terminator -> Codegen (Named Terminator)
terminator trm = do
  blk <- current
  modifyBlock (blk { term = Just trm })
  return trm

-------------------------------------------------------------------------------
-- Block Stack
-------------------------------------------------------------------------------

entry :: Codegen Name
entry = gets currentBlock

addBlock :: String -> Codegen Name
addBlock bname = do
  bls <- gets blocks
  ix <- gets blockCount
  nms <- gets names
  let new = emptyBlock ix
      (qname, supply) = uniqueName bname nms
  modify $ \s -> s { blocks = Map.insert (Name qname) new bls
                   , blockCount = ix + 1
                   , names = supply
                   }
  return (Name qname)

setBlock :: Name -> Codegen ()
setBlock bname =
  modify $ \s -> s { currentBlock = bname }

getBlock :: Codegen Name
getBlock = gets currentBlock

modifyBlock :: BlockState -> Codegen ()
modifyBlock new = do
  active <- gets currentBlock
  modify $ \s -> s { blocks = Map.insert active new (blocks s) }

current :: Codegen BlockState
current = do
  c <- gets currentBlock
  blks <- gets blocks
  case Map.lookup c blks of
    Just x -> return x
    Nothing -> throwError $ "No such block: " ++ show c

-------------------------------------------------------------------------------
-- Symbol Table
-------------------------------------------------------------------------------

assign :: String -> Operand -> Codegen ()
assign var x = do
  lcls <- gets symtab
  modify $ \s -> s { symtab = (var, x) : lcls }


-------------------------------------------------------------------------------
--- Types
-------------------------------------------------------------------------------

type TypedOperand = (AST.Type, AST.Operand)

checkType :: AST.Type -> TypedOperand -> Codegen AST.Operand
checkType ty (realTy, realVal) = do
  when (ty /= realTy) (throwError $ "Type error (expected: " ++ show ty ++ ", actual: " ++ show realTy ++ ")")
  return realVal

voidValue :: TypedOperand
voidValue = (AST.VoidType, cons $ C.Undef AST.VoidType)


-- References
local ::  Name -> Operand
local = LocalReference int64

varRef :: VId -> Type.Type -> Codegen Operand
varRef (VId x) ty = do
  isGlobal <- asks (Map.member x)
  if isGlobal then
    load $ cons $ C.GlobalReference (typeToLLVMType ty) (Name x)
  else
    load (LocalReference (typeToLLVMType ty) (Name x))

varPtr :: VId -> Type.Type -> Codegen Operand
varPtr (VId x) ty = do
  isGlobal <- asks (Map.member x)
  if isGlobal then
    return $ cons $ C.GlobalReference (typeToLLVMType ty) (Name x)
  else
    return $ LocalReference (typeToLLVMType ty) (Name x)

localVarPtr :: VId -> Type.Type -> Operand
localVarPtr (VId x) ty = LocalReference (typeToLLVMType ty) (Name x)

localVarRef :: VId -> Type.Type -> Codegen Operand
localVarRef (VId x) ty = load $ LocalReference (typeToLLVMType ty) (Name x)

-- Arithmetic and Constants
add :: Operand -> Operand -> Codegen Operand
add a b = instr $ Add False False a b []

sub :: Operand -> Operand -> Codegen Operand
sub a b = instr $ Sub False False a b []

mul :: Operand -> Operand -> Codegen Operand
mul a b = instr $ Mul False False a b []

div :: Operand -> Operand -> Codegen Operand
div a b = instr $ SDiv False a b []

cmp :: IP.IntegerPredicate -> Operand -> Operand -> Codegen Operand
cmp cond a b =
  boolToInt64 =<< instr (ICmp cond a b [])

eq :: AST.Operand -> AST.Operand -> Codegen AST.Operand
neq :: AST.Operand -> AST.Operand -> Codegen AST.Operand
lt :: AST.Operand -> AST.Operand -> Codegen AST.Operand
gt :: AST.Operand -> AST.Operand -> Codegen AST.Operand
le :: AST.Operand -> AST.Operand -> Codegen AST.Operand
ge :: AST.Operand -> AST.Operand -> Codegen AST.Operand

eq = cmp IP.EQ
neq = cmp IP.NE
lt = cmp IP.SLT
gt = cmp IP.SGT
le = cmp IP.SLE
ge = cmp IP.SGE


conditional :: Codegen TypedOperand -> Codegen TypedOperand -> Codegen TypedOperand -> Codegen TypedOperand
conditional cond etr efl = do
  ifthen <- addBlock "if.then"
  ifelse <- addBlock "if.else"
  ifexit <- addBlock "if.exit"
  -- branch
  ccond <- cond >>= checkType int64
  test <- cmp IP.EQ ccond (cons $ C.Int 64 0)
  _ <- cbr test ifelse ifthen -- Branch based on the condition
  -- if.then
  ------------------
  setBlock ifthen
  trval <- etr       -- Generate code for the true branch
  _ <- br ifexit                -- Branch to the merge block
  ifthenEnd <- getBlock
  -- if.else
  ------------------
  setBlock ifelse
  flval <- efl       -- Generate code for the true branch
  _ <- br ifexit                -- Branch to the merge block
  ifelseEnd <- getBlock
  -- if.exit
  ------------------
  setBlock ifexit
  phi [(trval, ifthenEnd), (flval, ifelseEnd)]

not0 :: TypedOperand -> Codegen TypedOperand
not0 op = conditional (return op) (return (int64, cons $ C.Int 64 1)) (return (int64, cons $ C.Int 64 0))

sand :: Codegen TypedOperand -> Codegen TypedOperand -> Codegen TypedOperand -- shortcut and
sor  :: Codegen TypedOperand -> Codegen TypedOperand -> Codegen TypedOperand  -- shortcut or

sand a b =
  conditional a (b >>= not0) (return (int64, cons $ C.Int 64 0))
    
sor a b = 
  conditional a (return (int64, cons $ C.Int 64 1)) (b >>= not0)



-- | zero-extension from boolean(i1) to int64
-- | false -> 0, true -> 1
boolToInt64 :: Operand -> Codegen Operand
boolToInt64 operand = instr $ ZExt operand int64 []

-- | Truncation from int64 to boolean(i1).
int64ToBool :: Operand -> Codegen Operand
int64ToBool operand = instr $ Trunc operand int1 [] 

cons :: C.Constant -> Operand
cons = ConstantOperand

ci32 :: Int32 -> Operand
ci32 v = ConstantOperand (C.Int 32 (fromIntegral v))

ci64 :: Int64 -> Operand
ci64 v = ConstantOperand (C.Int 64 (fromIntegral v))


toArgs :: [Operand] -> [(Operand, [A.ParameterAttribute])]
toArgs = map (\x -> (x, []))

-- Effects
call :: Operand -> [Operand] -> AST.Type -> Codegen (AST.Type, Operand)
call fn args retty = do
  val <- instr $ Call False CC.C [] (Right fn) (toArgs args) [] []
  return (retty, val)

alloca :: AST.Type -> Codegen Operand
alloca ty = instr $ Alloca ty Nothing 0 []

alloch :: AST.Type -> Codegen TypedOperand
alloch ty = do
  let malloc = ConstantOperand $ C.GlobalReference (AST.FunctionType (T.ptr T.i8) [T.i32]　False) (Name "malloc")
  (_, alloced) <- call malloc [cons $ C.Int 32 16 {- TODO Not correct -}] (T.ptr T.i8)
  ret <- instr $ BitCast alloced (T.ptr ty) []
  return (T.ptr ty, ret)
store :: Operand -> Operand -> Codegen ()
store ptr val = do
  instr $ Store False ptr val Nothing 0 []
  return ()

load :: Operand -> Codegen Operand
load ptr = instr $ Load False ptr Nothing 0 []

bitCast :: AST.Type -> Operand -> Codegen TypedOperand
bitCast ty obj = do
  i <- instr (BitCast obj ty [])
  return (ty, i)
-- Control Flow
br :: Name -> Codegen (Named Terminator)
br val = terminator $ Do $ Br val []

cbr :: Operand -> Name -> Name -> Codegen (Named Terminator)
cbr cond tr fl = do
  condBool <- int64ToBool cond
  terminator $ Do $ CondBr condBool tr fl []

arrayIndex :: Operand -> Operand -> Codegen Operand
arrayIndex a i = instr $ GetElementPtr False a [i] []

phi :: [((AST.Type, Operand), Name)] -> Codegen (AST.Type, Operand)
phi incoming = do
  when (null incoming) $ throwError $ "null phi node: " ++ show incoming
  let types = map (fst . fst) incoming
  when (any (/= head types) types) $ throwError $ "invalid phi node (type error):" ++ show incoming
  let ty = head types
  result <- instr $ Phi ty (map ( \((_ty, operand), nodeName) -> (operand, nodeName)) incoming) []
  return (ty, result)

ret :: Operand -> Codegen (Named Terminator)
ret val = terminator $ Do $ Ret (Just val) []

retNone :: Codegen (Named Terminator)
retNone = terminator $ Do $ Ret Nothing []


----
withContextT :: (Context -> ExceptT e IO a) -> ExceptT e IO a
withContextT action = liftIO (withContext (runExceptT . action)) >>= liftEither

liftEither :: MonadError e m => Either e a -> m a
liftEither (Left e) = throwError e
liftEither (Right val) = return val

liftError :: ExceptT String IO a -> IO a
liftError = runExceptT >=> either fail return

typeToLLVMType :: Type.Type -> AST.Type
typeToLLVMType ty = case ty of
  TUnit -> AST.VoidType
  TInt -> T.i32
  TFloat -> float
  TBool -> T.i1
  TFun ls ret -> T.ptr T.i8 {- type of general closure -}
  TArray a -> AST.PointerType (typeToLLVMType a) (AddrSpace 0)
  TTuple ls -> AST.StructureType False (map typeToLLVMType ls)
  TVar _ -> error "unresolved type variable"

funtypeToLLVMType :: Type.Type -> AST.Type
funtypeToLLVMType ty = case ty of
  TFun ls ret -> AST.FunctionType (typeToLLVMType ret) (map typeToLLVMType ls) False
  _ -> error "not a funtype"


codegenExpr :: ClosExp -> Codegen TypedOperand
codegenExpr (CUnit :-: _) = return voidValue
codegenExpr (CInt v :-: _) = do
  return (int32, ci32 $ fromIntegral v)
codegenExpr (CFloat f :-: _) = do
  return (float, cons $ C.Float $ F.Single f)
codegenExpr (CArithBin op (VId x) (VId y) :-: _) = do
  let opInst = fromJust $ List.lookup op [(Syntax.Add, add), (Syntax.Sub, sub), (Syntax.Mul, mul), (Syntax.Div, Emit.div)]
  ret <- join $ opInst <$> (localVarRef (VId x) Type.TInt) <*> (localVarRef (VId y) Type.TInt)
  return (int32, ret)
codegenExpr (CNeg (VId x) :-: _) = do
  ret <- join $ sub (ci32 0) <$> (load $ local $ Name x)
  return (int32, ret)
codegenExpr (CLet (VId x) ty e1 e2 :-: _) = do
  addInstr (Name x) (Alloca (typeToLLVMType ty) Nothing 0 [])
  (_, op1) <- codegenExpr e1
  _ <- store (local $ Name x) op1
  codegenExpr e2
codegenExpr (CVar x :-: ty) = do
  ret <- localVarRef x ty
  return (typeToLLVMType ty, ret)
codegenExpr (CMakeCls (VId x) t (Closure (LId topfunc) fvs) expr :-: _) = do
  formFV <- asks (formalFV . fromJust . Map.lookup x)
  let recType = T.StructureType False (T.ptr (funtypeToLLVMType t) : map (\(_,t) -> typeToLLVMType t) formFV)
  (_, ptr) <- alloch recType
  ptrFunc <- instr $ GetElementPtr True ptr [ci64 0, ci32 0] []
  store ptrFunc (cons (C.GlobalReference (funtypeToLLVMType t) (Name (topfunc))))
  -- set free variables
  forM_ [1..length formFV] $ \i -> do
    let nm = fvs !! (i-1)
    ptrElem <- instr $ GetElementPtr True ptr [ci64 0, ci32 (fromIntegral i)] []
    elem <- varRef nm (snd (formFV !! i))
    store ptrElem elem
  addInstr (Name x) (Alloca (typeToLLVMType t) Nothing 0 [])
  let xvar = localVarPtr (VId x) t
  (_, clos) <- bitCast (T.ptr T.i8) ptr
  store xvar clos
  codegenExpr expr
codegenExpr (CAppDir (LId x) args :-: ty) = do
  funTy@(TFun argTy _) <- asks (snd . Closure.name . fromJust . Map.lookup x)
  let fun = cons (C.GlobalReference (funtypeToLLVMType funTy) (Name (x ++ ".dir")))
  argOp <- mapM (\(x, t) -> varRef x t) (zip args argTy)
  call fun argOp (typeToLLVMType ty)
codegenExpr (CGet (VId x) (VId y) :-: ty) = do
  xv <- load $ local $ Name x
  yv <- load $ local $ Name y
  ret <- arrayIndex xv yv >>= load
  return (typeToLLVMType ty, ret)
codegenExpr (CPut (VId x) (VId y) (VId z) :-: ty) = do
  xv <- load $ local $ Name x
  yv <- load $ local $ Name y
  zv <- load $ local $ Name z
  addr <- arrayIndex xv yv
  _ <- store addr zv
  return voidValue

codegenTop :: [CFundef] -> ClosExp -> LLVM ()
codegenTop fundefs expr = do
  define (T.ptr T.i8) "malloc" [(T.i32, Name "size")] [] {- Functions with no blocks are treated as external functions -}
  mapM_ (emitFundef fundefs) fundefs
  (retty, codegenState) <- liftEither $ runCodegen fundefs $ do
      entryBlock <- addBlock entryBlockName
      _ <- setBlock entryBlock
      (ty, val) <- codegenExpr expr
      _ <- case ty of
        AST.VoidType -> retNone
        _            -> ret val
      return ty
  let blks = createBlocks codegenState
  define int32 "main" [] blks

emitFundef :: [CFundef] -> CFundef -> LLVM () 
emitFundef fundefs (CFundef (Id.VId idt, ty) arg formFV expr) =
  case ty of
    TFun _ retty -> do
      codegenState <- liftEither $ execCodegen fundefs $ do
          entryBlock <- addBlock entryBlockName
          _ <- setBlock entryBlock
          forM_ arg $ \(VId n, ty) -> do
            let ref = Name n
            addInstr ref (Alloca (typeToLLVMType ty) Nothing 0 [])
            store (localVarPtr (VId n) ty) (LocalReference (typeToLLVMType ty) (Name (n ++ ".ref")))
          (ty, val) <- codegenExpr expr
          _ <- case ty of
            AST.VoidType -> retNone
            _            -> ret val
          return ()
      let blks = createBlocks codegenState
      define (typeToLLVMType retty) (idt) (map (\(Id.VId n,t) -> (typeToLLVMType t, Name (n ++ ".ref"))) arg) blks {- stub -}
      when (null formFV) $
        define (typeToLLVMType retty) (idt ++ ".dir") (map (\(Id.VId n,t) -> (typeToLLVMType t, Name (n ++ ".ref"))) arg) blks {- stub -}
    _ -> throwError "not a function type"

emitAsm :: [CFundef] -> ClosExp -> IO String
emitAsm fns expr = liftError $ do
         newmod <- liftEither $ codegen (emptyModule "JITtest") fns expr
         optmod <- optimize newmod (Just 3)
         withContextT $ \ctx -> do
           _ <- withModuleFromAST ctx newmod $ \mm -> do
             liftIO $ putStrLn "***** Module before optimization *****"
             s <- liftIO $ moduleLLVMAssembly mm
             liftIO $ putStrLn s
           withModuleFromAST ctx optmod $ \mm -> do
             liftIO $ putStrLn "***** Optimized Module *****"
             s <- liftIO $ moduleLLVMAssembly mm
             liftIO $ putStrLn s
             return s


-- | Compiles 'fns' in the module 'mod' and returns new module.
codegen :: AST.Module -> [CFundef] -> ClosExp -> Either String AST.Module
codegen astmod fns expr = newast
  where
    modn    = codegenTop fns expr
    newast  = runLLVM astmod modn


-- | Optimizes a module. Optimization level is specified in parameter 'opt'.
optimize :: AST.Module -> Maybe Word -> ExceptT String IO AST.Module
optimize astmod opt =
  withContextT $ \context ->
    withModuleFromAST context astmod $ \m ->
      withPassManager (passes opt) $ \pm -> do
        -- Optimization Pass
        _ <- runPassManager pm m
        moduleAST m

-- | Returns passes whose level is 'opt'.
passes :: Maybe Word -> PassSetSpec
passes opt = defaultCuratedPassSetSpec { optLevel = opt }


