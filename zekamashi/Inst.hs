module Inst where

import Control.Monad (when)
import Prelude hiding (EQ)
import System.IO (Handle, hPutStrLn)
import Data.Binary.IEEE754 (floatToWord)
import Data.Word (Word32)
import Data.Bits

import AsmHelper

data RegImm =
  RIReg !Int | RIImm !Int
  deriving (Eq)

instance Show RegImm where
  show e = case e of
    RIReg i -> "$" ++ show i
    RIImm i -> show i

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

data ZekInst
  = Lda !Reg !Disp16 !Reg
  | Ldah !Reg !Disp16 !Reg
  | Ldl !Reg !Disp16 !Reg
  | Stl !Reg !Disp16 !Reg
  | BC !Cond !Reg !Label
  | Br !Reg !Label
  | Bsr !Reg !Label
  | Jmp !Reg !Reg
  | Jsr !Reg !Reg
  | Ret !Reg !Reg
  | FBC !Cond !FReg !Label
  | Addl !Reg !RegImm !Reg
  | Subl !Reg !RegImm !Reg
  | Cmp !Cmp !Reg !RegImm !Reg
  | And !Reg !RegImm !Reg
  | Sll !Reg !RegImm !Reg
  | Srl !Reg !RegImm !Reg
  | Lds !FReg !Disp16 !Reg
  | Sts !FReg !Disp16 !Reg
  | Cmps !Cmp !FReg !FReg !FReg
  | FOp !FOp !FReg !FReg !FReg
  | Invs !FReg !FReg
  | Sqrts !FReg !FReg
  | Itofs !Reg !FReg
  | Label !Label
  | Comment !String
  | ExtFile !FilePath

instance Show ZekInst where
  show e = case e of
    Lda a d b -> "\tLDA\t" ++ show a ++ ", " ++ show d ++ "(" ++ show b ++ ")"
    Ldah a d b -> "\tLDAH\t" ++ show a ++ ", " ++ show d ++ "(" ++ show b ++ ")"
    Ldl a d b -> "\tLDL\t" ++ show a ++ ", " ++ show d ++ "(" ++ show b ++ ")"
    Stl a d b -> "\tSTL\t" ++ show a ++ ", " ++ show d ++ "(" ++ show b ++ ")"
    BC c a d -> "\t" ++ show c ++ "\t" ++ show a ++ ", " ++ d
    Br a d -> "\tBR\t" ++ show a ++ ", " ++ d
    Bsr a d -> "\tBSR\t" ++ show a ++ ", " ++ d
    Jmp a b -> "\tJMP\t" ++ show a ++ ", (" ++ show b ++ ")"
    Jsr a b -> "\tJSR\t" ++ show a ++ ", (" ++ show b ++ ")"
    Ret a b -> "\tRET\t" ++ show a ++ ", (" ++ show b ++ ")"
    FBC c a d -> "\tF" ++ show c ++ "\t" ++ show a ++ ", " ++ d
    Addl a b c -> "\tADDL\t" ++ show a ++ ", " ++ show b ++ ", " ++ show c
    Subl a b c -> "\tSUBL\t" ++ show a ++ ", " ++ show b ++ ", " ++ show c
    Cmp op a b c -> "\tCMP" ++ show op ++ "\t" ++ show a ++ ", " ++ show b ++ ", " ++ show c
    And a b c -> "\tAND\t" ++ show a ++ ", " ++ show b ++ ", " ++ show c
    Sll a b c -> "\tSLL\t" ++ show a ++ ", " ++ show b ++ ", " ++ show c
    Srl a b c -> "\tSRL\t" ++ show a ++ ", " ++ show b ++ ", " ++ show c
    Lds a d b -> "\tLDS\t" ++ show a ++ ", " ++ show d ++ "(" ++ show b ++ ")"
    Sts a d b -> "\tSTS\t" ++ show a ++ ", " ++ show d ++ "(" ++ show b ++ ")"
    Cmps op a b c -> "\tCMPS" ++ show op ++ "\t" ++ show a ++ ", " ++ show b ++ ", " ++ show c
    FOp op a b c -> "\t" ++ show op ++ "\t" ++ show a ++ ", " ++ show b ++ ", " ++ show c
    Invs a b -> "\tINVS\t" ++ show a ++ ", " ++ show b
    Sqrts a b -> "\tSQRTS\t" ++ show a ++ ", " ++ show b
    Itofs a b -> "\tITOFS\t" ++ show a ++ ", " ++ show b
    Label l -> l ++ ":"
    Comment s -> "    # " ++ s ++ "\n"
    ExtFile filename -> "# warning: show_zek_inst for ExtFile. This part should be replaced with the content of the file:" ++ filename

emit_inst :: Handle -> ZekInst -> IO ()
emit_inst oc e = case e of
  ExtFile lib ->
    when (lib /= "") $ do
      cont <- readFile lib
      hPutStrLn oc cont
  inst -> hPutStrLn oc (show inst)
emit :: Handle -> [ZekInst] -> IO ()
emit oc code =  mapM_ (emit_inst oc) code

mov :: Reg -> Reg -> [ZekInst]
mov src dest = if src == dest then [] else [Lda dest 0 src]
fmov :: FReg -> FReg -> [ZekInst]
fmov src dest = if src == dest then [] else [FOp FOpAdd src (FReg 31) dest]
li :: Disp16 -> Reg -> ZekInst
li imm dest = Lda dest imm (Reg 31)

li32 :: Word32 -> Reg -> [ZekInst]
li32 imm dest = 
  let (yd, x) = decomposeWord32 imm in
  if yd == 0 then [Lda dest x (Reg 31)]
  else if x == 0 then
    [Ldah dest yd (Reg 31)]
  else
    [Lda dest x (Reg 31), Ldah dest yd dest]
lfi :: Float -> FReg -> [ZekInst]
lfi imm dest = li32 (floatToWord imm) rtmp ++ [Itofs rtmp dest]


abstAdd :: Reg -> Word32 -> Reg -> [ZekInst]
abstAdd rsrc imm rdest =
 let (x, y) = decomposeWord32 imm in
 if x == 0 then
   if y == 0 then []
   else [Lda rdest y rsrc]
 else
   if y == 0 then [Ldah rdest x rsrc]
   else [Lda rdest y rsrc, Ldah rdest x rdest]


-- | Decomposes unsigned 32-bit word to signed 16-bit integers.
-- | decomposeWord32 w returns (x, y) s.t. fromIntegral (x * 65536 + y) == w and x, y in [-0x8000, 0x7fff]
--
-- >>> Inst.decomposeWord32 100
-- (0,100)
-- >>> Inst.decomposeWord32 65537
-- (1,1)
-- >>> Inst.decomposeWord32 (-65537)
-- (-1,-1)
-- >>> Inst.decomposeWord32 (-2147483648)
-- (-32768,0)
-- >>> Inst.decomposeWord32 (2147483647)
-- (-32768,-1)
-- >>> Inst.decomposeWord32 (65535 * 32768)
-- (-32768,-32768)
-- >>> Inst.decomposeWord32 (65535 * 32767)
-- (32767,-32767)

decomposeWord32 :: Word32 -> (Disp16, Disp16)
decomposeWord32 w = let x = (fromIntegral ((w + 0x8000) `mod` 0x10000) - 0x8000 :: Disp16) in
  let y = (w - fromIntegral x) `shiftR` 16 in
  let yd = if y >= 0x8000 then fromIntegral y - 0x10000 else (fromIntegral y :: Disp16) in {- yd \in [-0x8000, 0x7fff] -}
  (yd, x)

