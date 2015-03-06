module Main where

import Control.Monad.Reader (runReader)
import Control.Monad (forM_, mapM)
import qualified Data.Map as Map
import System.Console.GetOpt
import System.Environment
import System.IO

import qualified MLexer
import Id
import Type
import MParser (parse)
import Typing
import KNormal (kNormal)
import Alpha (alpha)
import SSA
import SSAProp
import SSAFold
import SSAReduce
import SSACSE
import SSASimpl
import SSAElim
import Closure (CVardef, CFundef(..), trans)
import RegAlloc
import PhiElim
import Emit
import qualified Inst
import SSADom

data Config = Config { threshold :: !Int, limit :: !Int, glib :: ![String], outFile :: !(Maybe String) }

options :: [OptDescr (Config -> Config)]
options =
  [ Option [] ["inline"] (ReqArg (\s conf -> conf { threshold = read s }) "max size of inlining") "max size of inlined function"
  , Option [] ["iter"] (ReqArg (\s conf -> conf { limit = read s }) "opt iteration") "maximum number of optimizations iterated"
  , Option [] ["glib"] (ReqArg (\s conf -> conf { glib = s : glib conf }) "library") "ml libraries"
  , Option ['o'] ["output"] (ReqArg (\s conf -> conf { outFile = Just s }) "output") "output file"
  ] 

parseOpt :: [String] -> (Config, [String])
parseOpt argv =
  let (dat, nonOpts, errs) = getOpt Permute options argv in
  if null errs then
    (foldl (.) id dat (Config 0 1000 [] Nothing), nonOpts)
  else
    error ("error on parsing command line:" ++ show errs)

usage :: String
usage = "Hayashii MinCaml Compiler\n"
      ++ "usage: hmcc [--inline m] [--iter n] ...filenames without \".ml\"..."


extenv :: TypeEnv
extenv = Map.fromList
  [(Id "print_int", TFun [TInt] TUnit)
  ,(Id "create_array", TFun [TInt] (TArray TInt))
  ,(Id "create_float_array", TFun [TInt] (TArray TFloat))
  ,(Id "sin", TFun [TFloat] TFloat)
  ,(Id "cos", TFun [TFloat] TFloat)
  ,(Id "atan", TFun [TFloat] TFloat)
  ,(Id "sqrt", TFun [TFloat] TFloat)
  ,(Id "abs_float", TFun [TFloat] TFloat)
  ,(Id "int_of_float", TFun [TFloat] TInt)
  ,(Id "truncate", TFun [TFloat] TInt)
  ,(Id "float_of_int", TFun [TInt] TFloat)
  ,(Id "print_newline", TFun [TUnit] TUnit)
  ]

repl :: Config -> String -> IO ()
repl conf str = do
  let lexed = MLexer.lex str
  let syntax = parse lexed
  case syntax of
    Right syn -> do
　　　　　　let typed = either (error . show) id (typing extenv syn)
      let kn = kNormal extenv typed
      let al = alpha kn
      let (cexp, cfuns) = trans al
      let ssa = runReader (runCounterT (ssaTrans cfuns cexp))
            (Map.fromList (map (\(CFundef {Closure.name = (VId n,ty)}) -> (Id n, ty)) cfuns ++ [(Id ("min_caml_" ++ x), ty) | (Id x, ty) <- Map.assocs extenv]))
      putStrLn "ssa:"
      print ssa
      putStrLn "**** optimized SSA ****"
      let optSSA = iterate (cse . eliminate . simplify . reduce . constFold . propagate) ssa !! 10
      print optSSA
      putStrLn "*** Dominance ***"
      forM_ (map dominance optSSA) $ \x -> do
        print x
      let regSSA = regAlloc optSSA
      putStrLn "**** register-allocated SSA ****"
      print regSSA
      putStrLn "**** Phi-eliminated SSA ****"
      let peSSA = elimPhi regSSA
      print peSSA
      let insts = emit peSSA
      case outFile conf of
        Nothing -> mapM_ print insts
        Just x -> do
          handle <- openFile x WriteMode
          Inst.emit handle insts
          hClose handle
    Left x -> error x

processLib :: [String] -> IO (TypeEnv, [CVardef])
processLib = undefined


main :: IO ()
main = do
  argv <- getArgs
  let (conf, files) = parseOpt argv
  if null files then do
    putStrLn usage
    str <- getContents
    repl conf str
  else
    print files
    
