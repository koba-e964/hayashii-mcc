module Type where
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
  deriving (Eq, Show)

genType :: Type
genType = TVar ""

data Typed a = a :-: Type deriving (Eq, Show)

instance Functor Typed where
  fmap f (a :-: b) = f a :-: b

