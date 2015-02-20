{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Set
import qualified Data.Map as Map
import qualified Test.Framework as TF
import qualified Test.Framework.Providers.HUnit as TFH
import qualified Test.HUnit as TH
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
tests = [testEasy, testLoop]

(<==) :: VId -> Op -> Inst
a <== b = Inst (Just a) b
infix 1 <==

testEasy :: TF.Test
testEasy = testEq "easy_ssa" (analyzeLiveness fundef) dat
  where
  fundef = SSAFundef (LId "f" :-: TInt) [] [] [blk]
  blk = Block "entry"
    emptyPhi
    [ "a" <== SId (ci32 (0 :: Int))
    , "b" <== SArithBin Add (OpVar ("a" :-: TInt)) (ci32 (1 :: Int))
    , "c" <== SArithBin Add (OpVar ("a" :-: TInt)) (ci32 (2 :: Int))
    , "d" <== SArithBin Add (OpVar ("b" :-: TInt)) (OpVar ("c" :-: TInt))
    ] (TRet (OpVar ("d" :-: TInt)))
  dat = LiveInfo (Map.singleton "entry" blive)
  blive = BlockLive
    Map.empty
    [ InstLive empty (singleton "a")
    , InstLive (singleton "a") (fromList ["a", "b"])
    , InstLive (fromList ["a", "b"]) (fromList ["b", "c"])
    , InstLive (fromList ["b", "c"]) (singleton "d")
    ] (InstLive (singleton "d") empty)

testLoop :: TF.Test
testLoop = testEq "liveness_loop" (analyzeLiveness fundef) dat where
  fundef = SSAFundef (LId "f" :-: TInt) [] [] [blkEntry, blkL0, blkL1]
  blkEntry = Block "entry"
    emptyPhi
    [ "s" <== SId (ci32 (3 :: Int))
    ] (TJmp "l0")
  blkL0 = Block "l0"
    (Phi ["a"] (Map.fromList [("entry", [OpVar ("s" :-: TInt)]), ("l0", [OpVar ("a" :-: TInt)])]))
    [ "b" <== SCmpBin LE (OpVar ("a" :-: TInt)) (ci32 (1 :: Int))
    ] (TBr (OpVar ("b" :-: TInt)) "l1" "l0")
  blkL1 = Block "l1"
    (Phi [] (Map.singleton "l0" []))
    [] (TRet (OpVar ("a" :-: TInt)))
  dat = LiveInfo (Map.fromList [("entry", bliveEntry), ("l0", bliveL0), ("l1", bliveL1)])
  bliveEntry = BlockLive
    Map.empty
    [ InstLive empty (singleton "s")
    ] (InstLive (singleton "s") (singleton "s"))
  bliveL0 = BlockLive
    (Map.fromList [("entry", InstLive (singleton "s") (singleton "a")), ("l0", InstLive (singleton "a") (singleton "a"))])
    [ InstLive  (singleton "a") (fromList ["a", "b"])
    ] (InstLive (fromList ["a", "b"]) (singleton "a"))
  bliveL1 = BlockLive
    (Map.fromList [("l0", InstLive (singleton "a") (singleton "a"))])
    [] (InstLive (singleton "a") empty)

