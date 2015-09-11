{
{-# OPTIONS_GHC -w #-}
module MParser where

import Prelude hiding (EQ, LT, GT)
import Control.Applicative

import qualified MLexer
import MLexer hiding (lex)
import Syntax hiding (LE)
import qualified Syntax
import Id
import Type
}

%name      mparse     exp
%name      mlibparse     library
%tokentype {Token}
%error     {parseError} 
%monad {Either String} {(>>=)} {Right}

%token

BOOL {BOOL $$}
INT {INT $$}
FLOAT {FLOAT $$}
NOT {NOT}
"-" {MINUS}
"+" {PLUS}
"*" {AST}
"/" {SLASH}
"-." {MINUS_DOT}
"+." {PLUS_DOT}
"*." {AST_DOT}
"/." {SLASH_DOT}
"=" {EQ}
"<>" {NEQ}
"<=" {LE}
">=" {GE}
"<" {LT}
">" {GT}
IF {IF}
THEN {THEN}
ELSE {ELSE}
ID {ID $$}
LET {LET}
IN {IN}
REC {REC}
"," {COMMA}
ARRAY_CREATE {ARRAY_CREATE}
"." {DOT}
"<-" {LESS_MINUS}
";" {SEMICOLON}
"(" {LPAREN}
")" {RPAREN}
"_" {PLACEHOLDER}
%right prec_let
%right ";"
%right prec_if
%right "<-"
%left ","
%left prec_tuple_m
%left prec_tuple_s
%left "=" "<>" "<" ">" "<=" ">="
%left "+" "-" "+." "-."
%left "*." "/." "*" "/"
%right prec_unary_minus
%left prec_app
%left "."

%%

simple_exp: -- /* 括弧をつけなくても関数の引数になれる式 (caml2html: parser_simple) */
  "(" exp_tuple ")"
    { $2 }
| "(" ")"
    { Unit }
| BOOL
    { Bool $1 }
| INT
    { Int $1 }
| FLOAT
    { Float $1 }
| ID
    { Var $1 }
| simple_exp "." "(" exp ")"
    { Get $1 $4 }
;
exp: -- 一般の式 
  simple_exp
    { $1 }
| NOT simple_exp
    %prec prec_app
    { Not($2) }
| "-" simple_exp
    %prec prec_unary_minus
    { case $2 of {
      Float f -> Float (- f); -- -1.23などは型エラーではないので別扱い
       e -> Neg e ;
     } }
| exp "+" exp
    { ArithBin Add $1 $3 }
| exp "-" exp
    { ArithBin Sub $1 $3 }
| exp "*" exp
    { ArithBin Mul $1 $3 }
| exp "/" exp
    { ArithBin Div $1 $3 }
| exp "=" exp
    { Cmp Eq $1 $3 }
| exp "<>" exp
    { Not(Cmp Eq $1 $3) }
| exp "<" exp
    { Not(Cmp Syntax.LE $3 $1) }
| exp ">" exp
    { Not(Cmp Syntax.LE $1 $3) }
| exp "<=" exp
    { Cmp Syntax.LE $1 $3 }
| exp ">=" exp
    { Cmp Syntax.LE $3 $1 }
| IF exp THEN exp ELSE exp
    %prec prec_if
    { If $2 $4 $6 }
| "-." simple_exp
    %prec prec_unary_minus
    { FNeg $2  }
| exp "+." exp
    { FloatBin FAdd $1 $3 }
| exp "-." exp
    { FloatBin FSub $1 $3 }
| exp "*." exp
    { FloatBin FMul $1 $3 }
| exp "/." exp
    { FloatBin FDiv $1 $3 }
| LET ID "=" exp IN exp
    %prec prec_let
    { Let $2 Type.genType $4 $6 }
| LET REC fundef IN exp
    %prec prec_let
    { LetRec $3 $5 }
| simple_exp actual_args
    %prec prec_app
    { App $1 $2 }
| LET "(" pat ")" "=" exp IN exp %prec prec_let
    { LetTuple $3 $6 $8 }
| simple_exp "." "(" exp ")" "<-" exp
    { Put $1 $4 $7 }
| exp ";" exp
    { Let (Id "") TUnit $1 $3 } -- "" represents dummy variable.
| ARRAY_CREATE simple_exp simple_exp
    %prec prec_app
    { Array $2 $3 }
;

exp_tuple:
  exp
    { $1 }
| elems
    { Tuple $1 }
;

fundef:
  ID formal_args "=" exp
    { Fundef { name = addType $1, args = $2, body = $4 } }
;
formal_args:
  formal_arg formal_args
    { $1 : $2 }
| formal_arg
    { [$1] }
;
formal_arg:
  ID { addType $1 }
| "_" { (Id "", TUnit) }
actual_args:
  actual_args simple_exp
    %prec prec_app
    { $1 ++ [$2] }
| simple_exp
    %prec prec_app
    { [$1] }
;
elems:
  elems "," exp %prec prec_tuple_m
    { $1 ++ [$3] }
| exp "," exp %prec prec_tuple_s
    { [$1, $3] }
;
pat:
  pat "," ID
    { $1 ++ [addType $3] }
| ID "," ID
    { [addType $1, addType $3] }
;


library:
  declare
   { [$1] }
| library declare
   { $1 ++ [$2] }
;

declare:
  LET ID "=" exp %prec prec_let
   { VarDec $2 $4 }
| LET REC fundef %prec prec_let
   { FunDec $3 }
;
{

addType :: Id -> (Id, Type)
addType x = (x, Type.genType)

parseError :: [Token] -> Either String a
parseError toks = Left $ "parseError" ++ show toks

removeComments :: Int -> [Token] -> Either String [Token]
removeComments 0 [] = return [] 
removeComments _ [] = Left "Unclosed comment."
removeComments n (COMM_BEGIN : ls) = removeComments (n+1) ls
removeComments 0 (COMM_END : _) = Left "Unexpected COMMENT END \"*)\""
removeComments n (COMM_END : ls) = removeComments (n-1) ls
removeComments n (x : ls) = do
  rest <- removeComments n ls
  if n == 0 then return (x : rest) else return rest

parse :: [Token] -> Either String Syntax
parse toks = mparse =<< removeComments 0 toks

parseLib :: [Token] -> Either String Library
parseLib toks = mlibparse =<< removeComments 0 toks

}
