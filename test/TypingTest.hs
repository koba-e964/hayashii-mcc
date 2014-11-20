module Main where

import qualified Data.Map as Map
import System.Timeout
import qualified Test.Framework as TF
import qualified Test.Framework.Providers.HUnit as TFH 
import Syntax (Syntax)
import qualified MLexer
import qualified MParser
import Id
import Type
import Typing

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

checkType :: String -> Either TypingError Syntax
checkType expr = typing extenv (exprOfString expr)

checkValidType :: String -> String -> TF.Test
checkValidType name expr = TFH.testCase name $ timeoutFail time $
  case checkType expr of
    Left x -> error $ show x
    Right y -> y `seq` return ()

checkInvalidType :: String -> String -> TF.Test
checkInvalidType name expr = TFH.testCase name $ timeoutFail time $
  case checkType expr of
    Right _ -> error $ "expression \"" ++ expr ++ "\" should cause type error"
    Left _ -> return ()


tests :: [TF.Test]
testRec :: TF.Test
testFloat :: TF.Test

tests = [testRec, testFloat]

testRec = TF.testGroup "typing_rec"
  [ checkValidType "typing_rec1" "let rec fib x = if x <= 0 then x else fib (x-1) + fib (x-2) in print_int (fib 5)"
  , checkValidType "typing_rec2" "let rec fib x = if x <= 0 then x else fib (x-1) + fib (x-2) in fib 5"
  , checkInvalidType "typing_rec3" "let rec f xs = f (xs, 1) in print_int 1" -- occur check
  , checkInvalidType "typing_rec4" "let rec append x = append (x, x) in print_int 30"
  , checkValidType "typing_rec5" "let rec f x = f x in print_int 30"
  ]

testFloat = TF.testGroup "typing_float"
  [ checkValidType "typing_float1" "print_float (2. +. 3.)"
  , checkValidType "typing_float1" "print_float (sin 2. +. 3.)"
  ]

