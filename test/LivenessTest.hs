{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Set
import qualified Data.Map as Map
import System.Timeout
import qualified Test.Framework as TF
import qualified Test.Framework.Providers.HUnit as TFH
import qualified Test.HUnit as TH
import Control.Monad.Reader
import Syntax
import Id
import Type
import SSA
import SSALiveness

main :: IO ()
main = TF.defaultMain tests

testEq :: (Eq a, Show a) => String -> a -> a -> TF.Test
testEq msg actual expected = TFH.testCase msg (TH.assertEqual msg expected actual)

tests :: [TF.Test]
tests = [testEasy]


testEasy = TF.testGroup "liveness_easy"
  [ testEq "easy_ssa" (analyzeLiveness fundef) dat
  ] where
  fundef = SSAFundef (LId "f" :-: TInt) [] [] [blk]
  blk = Block "entry"
    [ Inst (Just "a") (SId (ci32 0))
    , Inst (Just "b") (SArithBin Add (OpVar ("a" :-: TInt)) (ci32 1))
    , Inst (Just "c") (SArithBin Add (OpVar ("a" :-: TInt)) (ci32 2))
    , Inst (Just "d") (SArithBin Add (OpVar ("b" :-: TInt)) (OpVar ("c" :-: TInt)))
    ] (TRet (OpVar ("d" :-: TInt)))
  dat = LiveInfo (Map.singleton "entry" blive)
  blive = BlockLive
    [ InstLive empty (singleton "a")
    , InstLive (singleton "a") (fromList ["a", "b"])
    , InstLive (fromList ["a", "b"]) (fromList ["b", "c"])
    , InstLive (fromList ["b", "c"]) (singleton "d")
    ] (InstLive (singleton "d") empty)

