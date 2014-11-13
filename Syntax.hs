module Syntax where

import qualified Id as Id
import Id (Id(..))
import qualified Type as Type

-- | Data type that represents MinCaml AST.
data Syntax
  = Unit
  | Bool !Bool
  | Int !Int
  | Float !Float
  | Not !Syntax
  | Neg !Syntax
  | ArithBin !ArithBinOp !Syntax !Syntax
  | FNeg !Syntax
  | FloatBin !FloatBinOp !Syntax !Syntax
  | Cmp !CmpOp !Syntax !Syntax
  | If !Syntax !Syntax !Syntax
  | Let !Id.Id !Type.Type !Syntax !Syntax
  | Var !Id.Id
  | LetRec !Fundef !Syntax
  | App !Syntax ![Syntax]
  | Tuple ![Syntax]
  | LetTuple ![(Id.Id, Type.Type)] !Syntax !Syntax
  | Array !Syntax !Syntax
  | Get !Syntax !Syntax
  | Put !Syntax !Syntax !Syntax
  deriving (Eq, Show)
data Fundef = Fundef { name :: !(Id.Id, Type.Type), args :: ![(Id.Id, Type.Type)], body :: !Syntax }
  deriving (Eq, Show)

data ArithBinOp
  = Add | Sub deriving (Eq, Show)
data CmpOp
  = Eq | LE deriving (Eq, Show)
data FloatBinOp
  = FAdd | FSub | FMul | FDiv deriving (Eq, Show)

type Library = [Declare]

data Declare
  = VarDec Id Syntax
  | FunDec Fundef
  deriving (Eq)

instance Show Declare where
  show (VarDec (Id i) e) = i ++ ":=" ++ show e
  show (FunDec f) = show f
