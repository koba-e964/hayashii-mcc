module Main where

import qualified Test.Framework as TF
import qualified Test.Framework.Providers.HUnit as TFH 
import qualified Test.HUnit as TH
import Syntax
import qualified MLexer
import qualified MParser
import Id
import qualified Type

main :: IO ()
main = TF.defaultMain tests

exprOfString :: String -> Syntax
exprOfString str = 
  let toks = MLexer.lex str
      exprOrErr = MParser.parse toks in
  case exprOrErr of
    Right x -> x
    Left  _ -> error $ "no parse: " ++ str

testEq :: (Eq a, Show a) => String -> a -> a -> TF.Test
testEq msg actual expected = TFH.testCase msg (TH.assertEqual msg expected actual)

checkInvalidExpr :: String -> String -> TF.Test
checkInvalidExpr name str = TFH.testCase name $ TH.assertBool "failure expected" $
  let toks = MLexer.lex str
      exprOrErr = MParser.parse toks in
  case exprOrErr of
    Right _ -> False
    Left  _ -> True

tests :: [TF.Test]
tests = [testOpPrecedence, testLet, testApp, testCmp]

testOpPrecedence :: TF.Test
testOpPrecedence = TF.testGroup "op_precedence"
 [ testEq "op_precedence1" (exprOfString "(1+2) *. (3 *. 4 + 5 *. 6)") (FloatBin FMul (ArithBin Add (Int 1) (Int 2)) (ArithBin Add (FloatBin FMul (Int 3) (Int 4)) (FloatBin FMul (Int 5) (Int 6))))
 , testEq "op_precedence2" (exprOfString "2*.5+3*.4") (ArithBin Add (FloatBin FMul (Int 2) (Int 5)) (FloatBin FMul (Int 3) (Int 4)))
 , testEq "op_precedence3" (exprOfString "let x = 3 in x + 4") (Let (Id "x") Type.genType (Int 3) (ArithBin Add (Var (Id "x")) (Int 4)))
 ]

testLet :: TF.Test
testLet = TF.testGroup "let"
  [ testEq "let1" (exprOfString "let x = 39 in x") (Let (Id "x") Type.genType (Int 39) (Var (Id "x")))
  , testEq "let2" (exprOfString "38; 4") (Let (Id "") Type.TUnit (Int 38) (Int 4))
  ]

testApp :: TF.Test
testApp = TF.testGroup "app"
  [ testEq "app1" (exprOfString "let x = () in x x") (Let (Id "x") Type.genType Unit (App (Var (Id "x")) [Var (Id "x")]))
  ]

testCmp :: TF.Test
testCmp = TF.testGroup "comparison"
  [ testEq "comparison1" (exprOfString "3 < 4") (Not (Cmp LE (Int 4) (Int 3)))
  , testEq "comparison2" (exprOfString "3 > 4") (Not (Cmp LE (Int 3) (Int 4)))
  , testEq "comparison3" (exprOfString "3 <= 4") (Cmp LE (Int 3) (Int 4))
  , testEq "comparison4" (exprOfString "3 >= 4") (Cmp LE (Int 4) (Int 3))
  , testEq "comparison5" (exprOfString "3 <> 4") (Not (Cmp Eq (Int 3) (Int 4)))
  , testEq "comparison6" (exprOfString "3 = 4") (Cmp Eq (Int 3) (Int 4))
  ]

testTuple :: TF.Test
testTuple = TF.testGroup "tuple"
  [ testEq "tuple1" (exprOfString "3, 4") (Tuple [Int 3, Int 4])
  , testEq "tuple2" (exprOfString "(3, 4, 8)") (Tuple [Int 3, Int 4, Int 8])
  , testEq "tuple3" (exprOfString "3, (4, 8)") (Tuple [Int 3, Tuple [Int 4, Int 8]])
  ]
