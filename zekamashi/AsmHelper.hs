module AsmHelper where

import Data.Int
import Prelude hiding (EQ)

import Id
-- Module for zekamashi-specific helper functions/definitions.

-- | Data type for general purpose registers(GPRs). They are named "$0" ... "$31". The content of $31 is always 0.
newtype Reg = Reg Int deriving (Eq) -- 0..31
-- | Data type for floating-point registers(FRs). They are named "$f0" ... "$f31". The content of $f31 is always 0.0.
newtype FReg = FReg Int deriving (Eq) -- 0..31

instance Show Reg where
  show (Reg i) = "$" ++ show i
instance Show FReg where
  show (FReg i) = "$f" ++ show i

newtype Var = Var String deriving (Eq)


data VarImm =
  VIReg !Int | VIImm !Int32
  deriving (Eq)

instance Show Var where
  show (Var x) = x

instance Show VarImm where
  show e = case e of
    VIReg i -> "$" ++ show i
    VIImm i -> show i


-- Special registers in Zekamashi.
-- | Register for addresses of closure.
rcl :: Reg
-- | Register for temporary values.
rtmp2 :: Reg
-- | Register for the heap pointer.
rhp :: Reg
-- | Register for temporary values.
rtmp :: Reg
-- | Register for link address (the address of caller).
rlr :: Reg
-- | Register for stack pointer. Stack is growing to 0xfffff.
rsp :: Reg
-- | Register for temporary floating-point values.
frtmp :: FReg

-- | Set of GPRs.
gregs :: [Reg]

-- | Set of floating-point registers.
fregs :: [FReg]

rcl = Reg 25
rtmp2 = Reg 26
rhp = Reg 27
rtmp = Reg 28
rlr = Reg 29
rsp = Reg 30
frtmp = FReg 30

gregs = [ Reg i | i <- [0 .. 24] ++ [26]]
fregs = [ FReg i | i <- [0 .. 30]]

data FOp = FOpAdd | FOpSub | FOpMul | FOpDiv deriving (Eq)
type Disp16 = Int
type Label = String
data Cond = EQ | NE | GE | LE deriving (Eq)

data Cmp = CEQ | CLE | CLT deriving (Eq)

instance Show FOp where
  show e = case e of
    FOpAdd -> "ADDS"
    FOpSub -> "SUBS"
    FOpMul -> "MULS"
    FOpDiv -> error "invalid float instruction: fdiv"

instance Show Cond where
  show e = case e of
    EQ -> "BEQ"
    NE -> "BNE"
    GE -> "BGE"
    LE -> "BLE"

instance Show Cmp where
  show e = case e of
    CEQ -> "EQ"
    CLE -> "LE"
    CLT -> "LT"


data VirFunc = VirFunc
  { virName :: !LId
  , virBlocks :: ![VirBlock]
  } deriving (Eq, Show)

data VirBlock = VirBlock
  { blockName :: !String
  , blockInst :: ![VirInst]
  } deriving (Eq, Show)

data VirFuncA a = VirFuncA
  { virNameA :: !LId
  , virBlocksA :: ![VirBlockA a]
  } deriving (Eq, Show)

data VirBlockA a = VirBlockA
  { blockNameA :: !String
  , blockInstA :: ![(VirInst, a)]
  } deriving (Eq, Show)


-- Virtual instruction
data VirInst
  = Li !Int32 !Var
  | Lfi !Float !Var
  | Ldl !Var !Disp16 !Var
  | Stl !Var !Disp16 !Var
  | BC !Cond !Var !Label !Label
  | Br !Label
  | Call !Label
  | FBC !Cond !Var !Label !Label
  | Add !Var !VarImm !Var
  | Sub !Var !Var !Var
  | Cmp !Cmp !Var !VarImm !Var
  | Sll !Var !VarImm !Var
  | Srl !Var !VarImm !Var
  | Lds !Var !Disp16 !Var
  | Sts !Var !Disp16 !Var
  | Cmps !Cmp !Var !Var !Var
  | FOp !FOp !Var !Var !Var
  | Invs !Var !Var
  | Sqrts !Var !Var
  | Ret !Var
  deriving (Eq, Show)

