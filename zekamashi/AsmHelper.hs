module AsmHelper where

-- Module for zekamashi-specific helper functions/definitions.

-- | Data type for general purpose registers(GPRs). They are named "$0" ... "$31". The content of $31 is always 0.
newtype Reg = Reg Int deriving (Eq) -- 0..31
-- | Data type for floating-point registers(FRs). They are named "$f0" ... "$f31". The content of $f31 is always 0.0.
newtype FReg = FReg Int deriving (Eq) -- 0..31

instance Show Reg where
  show (Reg i) = "$" ++ show i
instance Show FReg where
  show (FReg i) = "$f" ++ show i


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

