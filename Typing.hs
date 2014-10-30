module Typing where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import Debug.Trace

import Id
import Syntax
import Type

type TypeEnv = Map Id Type

data TypingError
  = UnifyError Type Type
  | TypingError Syntax Type Type
  | MiscError String Syntax
  deriving (Eq, Show)

type M = CounterT (ExceptT TypingError (StateT UnifyEnv (Reader TypeEnv)))

uniqType :: M Type
uniqType = TVar <$> (("T" ++) <$> (show <$> fresh))

type UnifyEnv = Map String Type


-- | If type variable x occurs in y, occur x y returns True.
occur :: String -> Type -> Bool
occur x (TVar y) = x == y
occur x (TFun ys y) = any (occur x) ys || occur x y
occur x (TTuple ys) = any (occur x) ys
occur x (TArray y) = occur x y
occur _ _ = False

unify :: Type -> Type -> M ()
unify x y | x == y = return ()
unify tx@(TFun xs x) ty@(TFun ys y) = do
  when (length xs /= length ys) (throwError (UnifyError tx ty))
  zipWithM_ unify xs ys
  unify x y
unify tx@(TTuple xs) ty@(TTuple ys) = do
  when (length xs /= length ys) (throwError (UnifyError tx ty))
  zipWithM_ unify xs ys
unify (TArray x) (TArray y) = unify x y
unify (TVar x) y = do
  when (occur x y) $ throwError $ UnifyError (TVar x) y
  hasX <- lift $ gets (Map.lookup x)
  case hasX of
    Just ty -> unify ty y
    Nothing -> do -- add to environment
      lift $ modify (Map.insert x y)
      return ()
unify x y@(TVar _) = unify y x
unify x y = throwError (UnifyError x y)

checkBinary :: Type -> Syntax -> Syntax -> M ()
checkBinary ty e1 e2 = do
  typingSub e1 >>= unify ty
  typingSub e2 >>= unify ty
  return ()
-- Type Inference
typingSub :: Syntax -> M Type
typingSub syn = case syn of
  Unit -> return TUnit
  Bool _ -> return TBool
  Int _ -> return TInt
  Float _ -> return TFloat
  Not x -> typingSub x >>= unify TBool >> return TBool
  Neg x -> typingSub x >>= unify TInt >> return TInt
  ArithBin _ e1 e2 -> checkBinary TInt e1 e2 >> return TInt
  FNeg x -> typingSub x >>= unify TFloat >> return TFloat
  FloatBin _ e1 e2 -> checkBinary TFloat e1 e2 >> return TFloat
  Cmp _ e1 e2 -> do
    ty1 <- typingSub e1
    ty2 <- typingSub e2
    unify ty1 ty2
    return TBool
  If e1 e2 e3 -> do
    typingSub e1 >>= unify TBool
    ty2 <- typingSub e2
    ty3 <- typingSub e3
    unify ty2 ty3
    return ty2
  Let x t e1 e2 -> do
    typingSub e1 >>= unify t
    local (Map.insert x t) (typingSub e2)
  Var x -> do
    env <- ask
    case Map.lookup x env of
      Just ty -> return ty
      Nothing -> throwError $ MiscError ("external variable " ++ show x ++ " was not in extenv.") (Var x)
  LetRec (Fundef { name = (x, ty), args = ls, body = e1}) e2 -> do
    newenv <- asks (Map.insert x ty)
    let argsenv = Map.fromList ls `Map.union` newenv
    TFun (map snd ls) <$> (local (const argsenv) (typingSub e1)) >>= unify ty
    local (const newenv) (typingSub e2)
  App e es -> do
    t <- uniqType
    join $ unify <$> typingSub e <*> (flip TFun t <$> mapM typingSub es);
    return t
  Tuple es -> TTuple <$> mapM typingSub es
  LetTuple xts e1 e2 -> do
    unify (TTuple (map snd xts)) =<< typingSub e1
    local (Map.fromList xts `Map.union`) (typingSub e2)
  Array e1 e2 -> do
    unify TInt =<< typingSub e1
    TArray <$> typingSub e2
  Get e1 e2 -> do
    t <- uniqType
    unify (TArray t) =<< typingSub e1
    unify TInt =<< typingSub e2
    return t
  Put e1 e2 e3 -> do
    t <- typingSub e3
    unify (TArray t) =<< typingSub e1
    unify TInt =<< typingSub e2
    return TUnit

-- | replace type variables with unique type variables
preprocess :: Syntax -> M Syntax
preprocess (Not e) = Not <$> preprocess e
preprocess (Neg e) = Neg <$> preprocess e
preprocess (ArithBin op e1 e2) = ArithBin op <$> preprocess e1 <*> preprocess e2
preprocess (FNeg e) = FNeg <$> preprocess e
preprocess (FloatBin op e1 e2) = FloatBin op <$> preprocess e1 <*> preprocess e2
preprocess (Cmp op e1 e2) = Cmp op <$> preprocess e1 <*> preprocess e2
preprocess (If e1 e2 e3) = If <$> preprocess e1 <*> preprocess e2 <*> preprocess e3
preprocess (Let x _ e1 e2) = do
  newty <- uniqType
  Let x newty <$> preprocess e1 <*> preprocess e2
preprocess (LetRec (Fundef { name = (x, _), args = a, body = b}) e) = do
  ty <- uniqType
  newargs <- mapM (\(x, _) -> (,) x <$> uniqType) a
  newb <- preprocess b
  LetRec (Fundef (x,ty) newargs newb) <$> preprocess e 
preprocess (App e1 es) = App <$> preprocess e1 <*> mapM preprocess es
preprocess (Tuple es) = Tuple <$> mapM preprocess es
preprocess (LetTuple ls e1 e2) = do
  newls <- mapM (\(x, _) -> (,) x <$> uniqType) ls
  LetTuple newls <$> preprocess e1 <*> preprocess e2
preprocess (Array e1 e2) = Array <$> preprocess e1 <*> preprocess e2
preprocess (Get e1 e2) = Get <$> preprocess e1 <*> preprocess e2
preprocess (Put e1 e2 e3) = Put <$> preprocess e1 <*> preprocess e2 <*> preprocess e3
preprocess x = return x

-- | Assignment
assign :: Syntax -> M Syntax
assign (Not e) = Not <$> assign e
assign (Neg e) = Neg <$> assign e
assign (ArithBin op e1 e2) = ArithBin op <$> assign e1 <*> assign e2
assign (FNeg e) = FNeg <$> assign e
assign (FloatBin op e1 e2) = FloatBin op <$> assign e1 <*> assign e2
assign (Cmp op e1 e2) = Cmp op <$> assign e1 <*> assign e2
assign (If e1 e2 e3) = If <$> assign e1 <*> assign e2 <*> assign e3
assign (Let x t e1 e2) = do
  newty <- assignType t
  Let x newty <$> assign e1 <*> assign e2
assign (LetRec (Fundef { name = (x, ty), args = a, body = b}) e) = do
  newty <- assignType ty
  newargs <- mapM (\(x, t) -> (,) x <$> assignType t) a
  newb <- assign b
  LetRec (Fundef (x, newty) newargs newb) <$> assign e 
assign (App e1 es) = App <$> assign e1 <*> mapM assign es
assign (Tuple es) = Tuple <$> mapM assign es
assign (LetTuple ls e1 e2) = do
  newls <- mapM (\(x, t) -> (,) x <$> assignType t) ls
  LetTuple newls <$> assign e1 <*> assign e2
assign (Array e1 e2) = Array <$> assign e1 <*> assign e2
assign (Get e1 e2) = Get <$> assign e1 <*> assign e2
assign (Put e1 e2 e3) = Put <$> assign e1 <*> assign e2 <*> assign e3
assign x = return x

assignType :: Type -> M Type
assignType (TVar x) = do
  hasX <- lift $ gets (Map.lookup x)
  case hasX of
    Just ty -> assignType ty
    Nothing -> trace ("Type variable " ++ x ++ " was assumed to be int\n") $ return TInt
assignType (TFun xs x) = TFun <$> mapM assignType xs <*> assignType x
assignType (TTuple xs) = TTuple <$> mapM assignType xs
assignType (TArray x) = TArray <$> assignType x
assignType x = return x

-- | extenv: type info of external functions
typing :: TypeEnv -> Syntax -> Either TypingError Syntax
typing extenv syn = runReader (evalStateT (runExceptT $ runCounterT $ do
    prsyn <- preprocess syn
    typingSub prsyn >>= unify TUnit
    assign prsyn
  ) Map.empty) extenv
