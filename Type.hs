module Type where

import qualified Data.List as List
-- Datatype that represents MinCaml types.
data Type
  = TUnit
  | TBool
  | TInt
  | TFloat
  | TFun ![Type] !Type -- arguments are uncurried
  | TTuple ![Type]
  | TArray !Type
  | TVar !String
  deriving (Eq)

instance Show Type where
  show ty = case ty of
    TUnit -> "unit"
    TBool -> "bool"
    TInt -> "int"
    TFloat -> "float"
    TFun ls ret -> List.intercalate " * " (map parenShow ls) ++ " -> " ++ parenShow ret
    TTuple ls -> "(" ++ List.intercalate ", " (map show ls) ++ ")"
    TArray a -> parenShow a ++ " array"
    TVar s -> "'" ++ s
   where
    parenShow t = case t of
      TFun {} -> "(" ++ show t ++ ")"
      _       -> show t

genType :: Type
genType = TVar ""

data Typed a = a :-: Type deriving (Eq, Show)

instance Functor Typed where
  fmap f (a :-: b) = f a :-: b


unType :: Typed a -> a
unType (x :-: _) = x

