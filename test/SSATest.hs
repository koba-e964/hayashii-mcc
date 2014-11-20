module Main where

import qualified Data.Map as Map
import System.Timeout
import qualified Test.Framework as TF
import qualified Test.Framework.Providers.HUnit as TFH 
import Control.Monad.Reader
import Syntax (Syntax)
import qualified MLexer
import qualified MParser
import Id
import Type
import Typing
import KNormal
import Alpha
import Closure
import SSA

main :: IO ()
main = TF.defaultMain tests

exprOfString :: String -> Syntax
exprOfString str = 
  let toks = MLexer.lex str
      exprOrErr = MParser.parse toks in
  case exprOrErr of
    Right x -> x
    Left  _ -> error $ "no parse: " ++ str

extenv :: TypeEnv
extenv = Map.fromList
  [ (Id "print_int", TFun [TInt] TUnit)
  , (Id "print_float", TFun [TFloat] TUnit)
  , (Id "int_float", TFun [TFloat] TInt)
  , (Id "sin", TFun [TFloat] TFloat)
  , (Id "cos", TFun [TFloat] TFloat)
  , (Id "tan", TFun [TFloat] TFloat)
  ]


time :: Int
time = 2000000 -- 2 seconds

timeoutFail :: Int -> IO a -> IO a
timeoutFail t m = do
  x <- timeout t m
  case x of
    Just y -> return y
    Nothing -> error "timeout"


ssaOfString :: String -> [SSAFundef]
ssaOfString str =
  let syn = exprOfString str in
　　let typed = either (error . show) id (typing extenv syn) in
  let kn = kNormal extenv typed in
  let al = alpha kn in
  let clos@(cexp, cfuns) = trans al in
  let ssa = runReader (runCounterT (ssaTrans cfuns cexp))
       (Map.fromList (map (\(CFundef {Closure.name = (VId n,ty)}) -> (Id n, ty)) cfuns) `Map.union` extenv) in
  ssa

checkSSA :: String -> String -> TF.Test
checkSSA name expr = TFH.testCase name $ void $ timeoutFail time $ 
   let res = ssaOfString expr in
   print res >> return res

tests = [testRec, testFloat]

testRec = TF.testGroup "typing_rec"
  [ checkSSA "typing_rec1" "let rec fib x = if x <= 0 then x else fib (x-1) + fib (x-2) in print_int (fib 5)"
  , checkSSA "typing_rec2" "let rec fib x = if x <= 0 then x else fib (x-1) + fib (x-2) in fib 5"
  , checkSSA "typing_rec5" "let rec f x = f x in print_int 30"
  ]

testFloat = TF.testGroup "typing_float"
  [ checkSSA "typing_float1" "print_float (2. +. 3.)"
  , checkSSA "typing_float1" "print_float (sin 2. +. 3.)"
  ]


