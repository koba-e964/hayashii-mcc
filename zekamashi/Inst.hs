module Inst where

import Control.Monad (when)
import Prelude hiding (EQ)
import System.IO (Handle, hPutStrLn)

newtype Reg = Reg Int deriving (Eq) -- 0..31
newtype FReg = FReg Int deriving (Eq) -- 0..31

data RegImm =
  RIReg !Int | RIImm !Int
  deriving (Eq)

instance Show Reg where
  show (Reg i) = "$" ++ show i
instance Show RegImm where
  show e = case e of
    RIReg i -> "$" ++ show i
    RIImm i -> show i

data Fop = FOpAdd | FOpSub | FOpMul | FOpDiv deriving (Eq, Show)
type Disp16 = Int
type Label = String
data Cond = EQ | NE | GE | LE deriving (Eq)

data Cmp = CEQ | CLE | CLT deriving (Eq)

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
  | Addl !Reg !RegImm !Reg
  | Subl !Reg !RegImm !Reg
  | Cmp !Cmp !Reg !RegImm !Reg
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
    Addl a b c -> "\tADDL\t" ++ show a ++ ", " ++ show b ++ ", " ++ show c
    Subl a b c -> "\tSUBL\t" ++ show a ++ ", " ++ show b ++ ", " ++ show c
    Cmp op a b c -> "\tCMP" ++ show op ++ "\t" ++ show a ++ ", " ++ show b ++ ", " ++ show c
    Label l -> l ++ ":"
    Comment s -> "    # " ++ s ++ "\n"
    ExtFile _ -> error "show_zek_inst for ExtFile"

emit_inst :: Handle -> ZekInst -> IO ()
emit_inst oc e = case e of
  ExtFile lib ->
    when (lib /= "") $ do
      cont <- readFile lib
      hPutStrLn oc cont
  inst -> hPutStrLn oc (show inst)
emit :: Handle -> [ZekInst] -> IO ()
emit oc code =  mapM_ (emit_inst oc) code

mov :: Reg -> Reg -> ZekInst
mov src dest = Lda dest 0 src
li :: Disp16 -> Reg -> ZekInst
li imm dest = Lda dest imm (Reg 31)

