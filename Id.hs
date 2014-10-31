module Id where

import Control.Monad.Identity (Identity)
import Control.Monad.State
import Data.String
import Type

-- Names of identifiers (both variables and/or globals).
newtype Id = Id String deriving (Eq, Ord)
-- Names of variables.
newtype VId = VId String deriving (Eq, Ord)
-- Names of top-level functions and/or global arrays.
newtype LId = LId String deriving (Eq, Ord)

instance Show Id where
  show (Id x) = x

instance Show VId where
  show (VId x) = "V:" ++ x

instance Show LId where
  show (LId x) = "L:" ++ x

instance IsString Id where
  fromString = Id
instance IsString VId where
  fromString = VId
instance IsString LId where
  fromString = LId

type CounterT = StateT Int

fresh :: Monad m => CounterT m Int
fresh = do
  x <- get
  put (x + 1)
  return x

genId :: Monad m => String -> CounterT m Id
genId str = do
  x <- get
  put (x + 1)
  return $ Id $ str ++ "." ++ show x

typeToId :: Type -> String
typeToId t = case t of
  TUnit -> "u"
  TBool -> "b"
  TInt -> "i"
  TFloat -> "d"
  TFun {} -> "f"
  TTuple _ -> "t"
  TArray _ -> "a" 
  TVar _ -> undefined

genTmp :: Monad m =>  Type -> CounterT m Id
genTmp ty = do
  x <- get
  put (x + 1)
  return $ Id $ "T" ++ typeToId ty ++ show x

runCounter :: CounterT Identity a -> a
runCounter m = evalState m 0
runCounterT :: Monad m => CounterT m a -> m a
runCounterT m = evalStateT m 0
