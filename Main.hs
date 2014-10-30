module Main where

import qualified Data.Map as Map
import System.Console.GetOpt
import System.Environment

import qualified MLexer
import Id
import Type
import MParser (parse)
import Typing
import KNormal (kNormal)
import Alpha (alpha)
import Closure (trans)


data Config = Config { threshold :: Int, limit :: Int }

options :: [OptDescr (Config -> Config)]
options =
  [ Option [] ["inline"] (ReqArg (\s conf -> conf { threshold = read s }) "max size of inlining") "max size of inlined function"
  , Option [] ["iter"] (ReqArg (\s conf -> conf { limit = read s }) "opt iteration") "maximum number of optimizations iterated"
  ] 

parseOpt :: [String] -> (Config, [String])
parseOpt args =
  let (dat, nonOpts, errs) = getOpt Permute options args in
  if null errs then
    (foldl (.) id dat (Config 0 1000), nonOpts)
  else
    error ("error on parsing command line:" ++ show errs)

usage :: String
usage = "MinCaml on Haskell\n"
      ++ "usage: min-caml [--inline m] [--iter n] ...filenames without \".ml\"..."


extenv :: TypeEnv
extenv = Map.fromList [(Id "print_int", TFun [TInt] TUnit)]

repl :: String -> IO ()
repl str = do
  let lexed = MLexer.lex str
  print lexed
  let syntax = parse lexed
  putStrLn "AST:"
  print syntax
  case syntax of
    Right syn -> do
　　　　　　let typed = either (error . show) id (typing extenv syn)
      putStrLn "Typed AST:"
      print typed
      let kn = kNormal typed
      putStrLn "k-normal form:"
      print kn
      let al = alpha kn
      print al
      let clos = trans al
      print clos
    Left x -> error x

main :: IO ()
main = do
  args <- getArgs
  let (conf, files) = parseOpt args
  if null files then do
    putStrLn usage
    str <- getContents
    repl str
  else
    print files
    
