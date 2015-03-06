module Closure where

import Control.Applicative
import Control.Monad.State
import Data.Set (Set, difference, union)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Debug.Trace

import Id
import Type
import Syntax hiding (name, args, body)
import KNormal hiding (freeVars, name, args, body)
import qualified KNormal
-- | Type of closure.
data Closure = Closure { entry :: LId, actualFV :: [VId] } deriving (Eq, Show)

-- Expression after closure transformation
type ClosExp = Typed ClosExpT

data ClosExpT
  = CUnit
  | CInt !Int
  | CFloat !Float
  | CNeg !VId
  | CArithBin !ArithBinOp !VId !VId
  | CFNeg !VId
  | CFloatBin !FloatBinOp !VId !VId
  | CIf !CmpOp !VId !VId !ClosExp !ClosExp -- 比較 + 分岐
  | CLet !VId !Type !ClosExp !ClosExp
  | CVar !VId
  | CMakeCls !VId !Type !Closure !ClosExp
  | CAppCls !VId ![VId]
  | CAppDir !LId ![VId]
  | CTuple ![VId]
  | CLetTuple ![(VId, Type)] !VId !ClosExp
  | CGet !VId !VId
  | CPut !VId !VId !VId
  | CExtArray !LId
  deriving (Eq, Show)
data CFundef = CFundef
    { name :: !(VId, Type)
    , args :: ![(VId, Type)]
    , formalFV :: ![(VId, Type)]
    , body :: !ClosExp
    } deriving (Eq, Show)
data CVardef = CVardef !Id !Type ClosExp


data Prog = Prog ![CVardef] ![CFundef] !ClosExp

freeVars :: ClosExp -> Set VId
freeVars (expr :-: _) = case expr of
    CUnit -> Set.empty
    (CInt {}) -> Set.empty
    (CFloat {}) -> Set.empty
    (CNeg x) -> Set.singleton x
    (CArithBin _ x y) -> Set.fromList [x, y]
    (CFNeg x) -> Set.singleton x
    (CFloatBin _ x y) -> Set.fromList [x, y]
    (CIf _ x y e1 e2) -> Set.fromList [x, y] `union` freeVars e1 `union` freeVars e2
    (CLet x _t e1 e2) -> freeVars e1 `union` Set.delete x (freeVars e2)
    (CVar x) -> Set.singleton x
    (CMakeCls x _t (Closure { actualFV = ys }) e) -> Set.delete x (Set.fromList ys `union` freeVars e)
    (CAppCls x ls) -> Set.fromList (x : ls)
    (CAppDir _ ls) -> Set.fromList ls
    (CTuple ls) -> Set.fromList ls
    (CLetTuple ls x e) -> Set.insert x (freeVars e `difference` Set.fromList (map fst ls))
    (CGet x y) -> Set.fromList [x, y]
    (CPut x y z) -> Set.fromList [x, y, z]
    (CExtArray {}) -> Set.empty

idToVId :: Id -> VId
idToVId (Id x) = VId x

idToLId :: Id -> LId
idToLId (Id x) = LId x

transSub :: Map VId Type -> Set Id -> KNormal -> State [CFundef] ClosExp
transSub env known (e :-: exprt) = fmap (:-: exprt) $ case e of
  KUnit -> return CUnit
  (KInt i) -> return $ CInt i
  (KFloat f) -> return $ CFloat f
  (KNeg (Id x)) -> return $ CNeg (VId x)
  (KArithBin op (Id x) (Id y)) -> return $ CArithBin op (VId x) (VId y)
  (KFNeg (Id x)) -> return $ CFNeg (VId x)
  (KFloatBin op (Id x) (Id y)) -> return $ CFloatBin op (VId x) (VId y)
  KIf cmp (Id x) (Id y) e1 e2 -> CIf cmp (VId x) (VId y) <$> transSub env known e1 <*> transSub env known e2
  KLet (Id x) ty e1 e2 -> CLet (VId x) ty <$> transSub env known e1 <*> transSub (Map.insert (VId x) ty env) known e2
  KVar (Id x) -> return $ CVar (VId x)
  KLetRec KFundef{ KNormal.name = (x@(Id n), ty), KNormal.args = yts, KNormal.body = e1 } e2 -> -- 関数定義の場合
      {- 関数定義let rec x y1 ... yn = e1 in e2の場合は、
	 xに自由変数がない(closureを介さずdirectに呼び出せる)
	 と仮定し、knownに追加してe1をクロージャ変換してみる -}
    do
      toplevel_backup <- get
      let env' = Map.insert (VId n) ty env
      let known' = Set.insert x known
      let newenv = Map.fromList (map (\(Id y, z) -> (VId y, z)) yts) `Map.union` env'
      e1' <- transSub newenv known' e1 
      {- 本当に自由変数がなかったか、変換結果e1'を確認する
       注意: e1'にx自身が変数として出現する場合はclosureが必要!
         (thanks to nuevo-namasute and azounoman; test/cls-bug2.ml参照) -}
      let zs = freeVars e1' `difference` Set.fromList (map (idToVId . fst) yts)
      (known'2, e1'2) <-
        if Set.null zs then return (known', e1') else do
        {- 駄目だったら状態(toplevelの値)を戻して、クロージャ変換をやり直す -}
        trace ("free variable(s) " ++ show zs ++ " found in function " ++ show x ++ "@.") $!
         trace ("function " ++ show x ++ " cannot be directly applied in fact@.") $ return ()
        put toplevel_backup
        e1'' <- transSub newenv known e1
        return (known, e1'')
      let zs' = Set.toList (freeVars e1' `difference` Set.map idToVId (Set.insert x (Set.fromList (map fst yts))))  -- 自由変数のリスト
      let zts = map (\z -> (z, fromJust (Map.lookup z env'))) zs' -- ここで自由変数zの型を引くために引数envが必要
      modify (CFundef{ name = (idToVId x, ty), args = map (\(Id y, z) -> (VId y, z)) yts, formalFV = zts, body = e1'2 } :) -- トップレベル関数を追加
      e2'@(e2'cont :-: _) <- transSub env' known'2 e2
      return $! if Set.member (idToVId x) (freeVars e2') then -- xが変数としてe2'に出現するか
        CMakeCls (idToVId x) ty Closure{ entry = idToLId x, actualFV = zs' } e2' -- 出現していたら削除しない
      else
	trace ("eliminating closure(s) " ++ show x ++ "@.") e2'cont -- 出現しなければMakeClsを削除
  KApp (Id x) ys | Set.member (Id x) known -> -- 関数適用の場合
      return $! trace ("directly applying " ++ x ++ "@.") (CAppDir (LId x) (map idToVId ys))
  KApp (Id f) xs -> return $ CAppCls (VId f) (map idToVId xs)
  KTuple xs -> return $ CTuple (map idToVId xs)
  KLetTuple xts (Id y) e1 -> let transXts = map (\(Id x, t) -> (VId x, t)) xts in
      CLetTuple transXts (VId y) <$> transSub (Map.fromList transXts `Map.union` env) known e1
  KGet (Id x) (Id y) -> return $ CGet (VId x) (VId y)
  KPut (Id x) (Id y) (Id z) -> return $ CPut (VId x) (VId y) (VId z)
  KExtArray (Id x) -> return $ CExtArray (LId x)
  KExtFunApp (Id x) ys -> return $ CAppDir (LId ("min_caml_" ++ x)) (map idToVId ys) -- add prefix "min_caml_" for external functions

trans :: KNormal -> (ClosExp, [CFundef])
trans expr = runState (transSub Map.empty Set.empty expr) []
