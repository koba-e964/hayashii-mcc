{-# LANGUAGE FlexibleContexts #-}
module KNormal where

import Control.Monad
import Control.Monad.Reader
import Data.Set (Set, difference, union)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Debug.Trace

import Id (CounterT, Id(Id), runCounterT)
import qualified Id as Id
import Type
import Typing
import Syntax hiding (name, args, body)
import qualified Syntax

-- Expression after k-normalization
data KNormal
  = KUnit
  | KInt !Int
  | KFloat !Float
  | KNeg !Id
  | KArithBin !ArithBinOp !Id !Id
  | KFNeg !Id
  | KFloatBin !FloatBinOp !Id !Id
  | KIf !CmpOp !Id !Id !KNormal !KNormal -- 比較 + 分岐
  | KLet !Id !Type !KNormal !KNormal
  | KVar !Id
  | KLetRec !KFundef !KNormal
  | KApp !Id ![Id]
  | KTuple ![Id]
  | KLetTuple ![(Id, Type)] !Id !KNormal
  | KGet !Id !Id
  | KPut !Id !Id !Id
  | KExtArray !Id
  | KExtFunApp !Id ![Id]
  deriving (Eq, Show)
data KFundef = KFundef { name :: !(Id, Type), args :: ![(Id, Type)], body :: !KNormal }
  deriving (Eq, Show)

-- | Free Variables in expression(KNormal). Note that external function names are not treated as free.
freeVars :: KNormal -> Set Id
freeVars KUnit = Set.empty
freeVars (KInt {}) = Set.empty
freeVars (KFloat {}) = Set.empty
freeVars (KNeg x) = Set.singleton x
freeVars (KArithBin _ x y) = Set.fromList [x,y]
freeVars (KFNeg x) = Set.singleton x
freeVars (KFloatBin _ x y) = Set.fromList [x,y]
freeVars (KIf _ x y e1 e2) = Set.fromList [x,y] `union` freeVars e1 `union` freeVars e2
freeVars (KLet name_ _ e1 e2) = Set.delete name_ (freeVars e2) `union` freeVars e1
freeVars (KVar x) = Set.singleton x
freeVars (KLetRec (KFundef {name = (x,_), args = a, body = b}) expr) =
  let zs = freeVars b `difference` Set.fromList (map fst a) in
  (zs `union` freeVars expr) `difference` Set.singleton x
freeVars (KApp x y) = Set.singleton x `union` Set.fromList y
freeVars (KTuple xs) = Set.fromList xs
freeVars (KLetTuple decs i e) = Set.singleton i `union` (freeVars e `difference` Set.fromList (map fst decs))
freeVars (KGet x y) = Set.fromList [x,y]
freeVars (KPut x y z) = Set.fromList [x,y,z]
freeVars (KExtArray {}) = Set.empty
freeVars (KExtFunApp _ xs) = Set.fromList xs -- external function name is not free

-- inserts let
insertLet :: Monad m => (KNormal, Type) -> (Id -> CounterT m (KNormal, a)) -> CounterT m (KNormal, a)
insertLet (e, t) k =
  case e of
    KVar x -> k x
    _ -> do
      x <- Id.genTmp t
      (e', t') <- k x
      return (KLet x t e e', t')


-- knormal routine. This routine normalizes expressions and adds types to them.
kNormalSub :: (Monad m, MonadReader TypeEnv m) => Map Id Type -> Syntax -> CounterT m (KNormal, Type)
kNormalSub env expr = case expr of
  Unit -> return (KUnit, TUnit)
  Bool b -> return (KInt (if b then 1 else 0), TInt)
  Int i  -> return (KInt i, TInt)
  Float f  -> return (KFloat f, TFloat)
  Not e    -> kNormalSub env (If e (Bool False) (Bool True))
  Neg e    -> do
      result <- kNormalSub env e
      insertLet result
        (\x -> return (KNeg x, TInt))
  ArithBin op e1 e2 -> do
      k1 <- kNormalSub env e1
      k2 <- kNormalSub env e2
      insertLet k1
        (\x -> insertLet k2
            (\y -> return (KArithBin op x y, TInt)))
  FNeg e    -> do
      result <- kNormalSub env e
      insertLet result
        (\x -> return (KFNeg x, TInt))
  FloatBin op e1 e2 -> do
      k1 <- kNormalSub env e1
      k2 <- kNormalSub env e2
      insertLet k1
        (\x -> insertLet k2
            (\y -> return (KFloatBin op x y, TInt)))
  cmp@Cmp {} ->
      kNormalSub env (If cmp (Bool True) (Bool False))
  If (Not x) e1 e2 -> kNormalSub env (If x e2 e1) -- condition with not
  If (Cmp cmp e1 e2) etr efl -> do -- condition with comparison
      k1 <- kNormalSub env e1
      k2 <- kNormalSub env e2
      insertLet k1
        (\x -> insertLet k2
            (\y -> do
               (etr', ttr) <- kNormalSub env etr
               (efl', _tfl) <- kNormalSub env efl
               return (KIf cmp x y etr' efl', ttr)
            ))
  If e1 etr efl -> kNormalSub env (If (Cmp Eq e1 (Bool False)) etr efl) -- others
  Let x t e1 e2 -> do
      (e1', _t1) <- kNormalSub env e1
      (e2', t2) <- kNormalSub (Map.insert x t env) e2
      return (KLet x t e1' e2', t2)
  Var x -> case Map.lookup x env of
        Just ty -> return (KVar x, ty)
        Nothing -> error "not impl array" -- reference to external array. Currently not implemented.
  LetRec (Fundef { Syntax.name = (x, t), Syntax.args = yts, Syntax.body = e1 }) e2 -> do
      let env' = Map.insert x t env
      (e2', t2) <- kNormalSub env' e2
      (e1', _t1) <- kNormalSub (Map.fromList yts `Map.union` env') e1
      return (KLetRec (KFundef { name = (x, t), args = yts, body = e1' }) e2', t2)
  App (Var f) e2s | Map.notMember f env -> do
    maybeTy <- asks (Map.lookup f)
    case maybeTy of
      Just (TFun _ t) -> 
        let bind xs ls = case ls of
              [] -> return (KExtFunApp f xs, t)
              e2 : e2s -> do
                sub <- kNormalSub env e2
                insertLet sub (\x -> bind (xs ++ [x]) e2s) in
          bind [] e2s
      _ -> error "error 138-30"
  App e1 e2s -> do
      result <- kNormalSub env e1
      trace ("result = " ++ show result) $ case result of
        g_e1@(_, TFun _ t) ->
          insertLet g_e1
            (\f ->
              let bind xs ess = case ess of -- "xs" are identifiers for the arguments
                   [] -> return (KApp f xs, t)
                   e2 : rest -> do
                    res <- kNormalSub env e2
                    insertLet res (\x -> bind (xs ++ [x]) rest) in
              bind [] e2s)  -- left-to-right evaluation
        _ -> error "error 3184"
  Tuple es ->
      let bind xs ts ess = case ess of -- "xs" and "ts" are identifiers and types for the elements
            [] -> return (KTuple xs, TTuple ts)
            e : rest -> do
              g_e@(_, t) <- kNormalSub env e
              insertLet g_e (\x -> bind (xs ++ [x]) (ts ++ [t]) rest) in
      bind [] [] es
  LetTuple xts e1 e2 -> do
      k1 <- kNormalSub env e1
      insertLet k1
        (\ y -> do
          (e2', t2) <- kNormalSub (Map.fromList xts `Map.union` env) e2
          return (KLetTuple xts y e2', t2))
  Array e1 e2 -> do
      k1 <- kNormalSub env e1
      insertLet k1
        (\ x -> do
          g_e2@(_, t2) <- kNormalSub env e2
          insertLet g_e2
            (\ y ->
              let l =
                   case t2 of
                    TFloat -> "create_float_array"
                    _ -> "create_array" in
              return (KExtFunApp (Id l) [x, y], TArray t2)))
  Get e1 e2 -> do
      res <- kNormalSub env e1
      case res of
        g_e1@(_, TArray t) -> do
          res2 <- kNormalSub env e2
          insertLet g_e1
            (\ x -> insertLet res2
                (\ y -> return (KGet x y, t)))
        _ -> error "error in knormal-get"
  Put e1 e2 e3 -> do
      k1 <- kNormalSub env e1
      k2 <- kNormalSub env e2
      k3 <- kNormalSub env e3
      insertLet k1
        (\ x -> insertLet k2
            (\ y -> insertLet k3
                (\ z -> return (KPut x y z, TUnit))))

kNormalM :: Monad m => Syntax -> CounterT (ReaderT TypeEnv m) KNormal
kNormalM e = liftM fst (kNormalSub Map.empty e)

kNormal :: TypeEnv -> Syntax -> KNormal
kNormal extenv e = runReader (runCounterT (kNormalM e)) extenv

