{-# LANGUAGE FlexibleContexts #-}
module Alpha where

import Control.Applicative
import Control.Monad.Reader
import Data.Map (Map)
import qualified Data.Map as Map

import Id
import Type
import KNormal

type Env = Map Id Id -- map from old name to new (unique) name. 

-- returns the unique name associated to x, or x if not found.
find :: MonadReader Env m => Id -> m Id
find x = do
  env <- ask
  case Map.lookup x env of
    Just y  -> return y
    Nothing -> return x

-- | A list of (originalID, type) ==> a list of (originalID, changedID, type)
changeBindings :: [(Id, Type)] -> CounterT (Reader Env) [(Id, Id, Type)]
changeBindings yts =
  forM yts $ \(i@(Id n), aty) -> do
    newi <- genId n
    return (i, newi, aty)

alphaSub :: KNormal -> CounterT (Reader Env) KNormal
alphaSub (expr :-: t) = fmap (:-: t) (case expr of
  KUnit -> return KUnit
  k@KInt {} -> return k
  k@KFloat {} -> return k
  KNeg x -> KNeg <$> find x
  KArithBin op x y -> KArithBin op <$> find x <*> find y
  KFNeg x -> KFNeg <$> find x
  KFloatBin op x y -> KFloatBin op <$> find x <*> find y
  KIf cmp a b e1 e2 -> KIf cmp <$> find a <*> find b <*> alphaSub e1 <*> alphaSub e2
  KLet x@(Id n) t e1 e2 -> do
    newid <- genId n
    ae1 <- alphaSub e1
    ae2 <- local (Map.insert x newid) (alphaSub e2)
    return $ KLet newid t ae1 ae2
  KVar x -> KVar <$> find x
  KLetRec (KFundef {name = (Id x, ty), args = yts, body = e1}) e2 -> do
    newid <- genId x
    newenv <- asks (Map.insert (Id x) newid) -- e1 is evaluated in newenv
    argsmap <- changeBindings yts
    let argenv = Map.fromList (map (\(a,b,_) -> (a,b)) argsmap) `Map.union` newenv -- e2 is evaluated in argenv.
    ae1 <- local (const argenv) $ alphaSub e1
    ae2 <- local (const newenv) $ alphaSub e2
    return $ KLetRec (KFundef (newid, ty) (map (\(_,b,c) -> (b,c)) argsmap) ae1) ae2
  KApp f args_ -> KApp <$> find f <*> mapM find args_
  KTuple elems -> KTuple <$> mapM find elems
  KLetTuple bindings x e -> do
    bmap <- changeBindings bindings
    KLetTuple (map (\(_,b,c) -> (b, c)) bmap) <$> find x 
      <*> local (Map.fromList (map (\(a,b,_) -> (a,b)) bmap) `Map.union`) (alphaSub e)
  KGet x y -> KGet <$> find x <*> find y
  KPut x y z -> KPut <$> find x <*> find y <*> find z
  k@KExtArray {} -> return k -- external symbols are not changed
  KExtFunApp x args_ -> KExtFunApp x <$> mapM find args_
  )
-- alpha-conversion of expression (making variable names unique)
alpha :: KNormal -> KNormal
alpha expr = runReader (runCounterT (alphaSub expr)) Map.empty

