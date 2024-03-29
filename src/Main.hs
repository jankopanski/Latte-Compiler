module Main where

import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import Control.Monad (when, void)

import Parser.ParLatte
import Parser.PrintLatte
import Parser.ErrM
import Frontend.Globals (putError)
import Frontend.TypeControl (runTypeControl)
import Frontend.StaticEvaluation (runStaticEvaluation)
import Frontend.ReturnEvaluation (runReturnEvaluation)
import Frontend.StringEvaluation (runStringEvaluation)
import Backend.IntermediateCodeGeneration (runIntermediateCodeGeneration)
import Backend.Optimisations (optimise)
import Backend.AssemblyGeneration (generateAssembly, generateFile)

type Verbosity = Int

putStrV :: Verbosity -> String -> IO ()
putStrV v s = when (v > 1) $ putStrLn s

showTree :: (Show a, Print a) => Int -> a -> IO ()
showTree v tree = do
  putStrV v $ "[Abstract Syntax]\n" ++ show tree
  putStrV v $ "[Linearized tree]\n" ++ printTree tree

usage :: IO ()
usage = putStrLn $ unlines
  [ "usage: Call with one of the following argument combinations:"
  , "  --help         Display this help message."
  , "  (file)         Parse content of files verbosely."
  , "  -v (file)      Verbose mode."
  ]

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> usage >> exitFailure
    ["--help"] -> usage >> exitSuccess
    "-v":[f] -> run 2 f
    [f] -> run 0 f
    _ -> putStrLn "Invalid flag or number of files"

run :: Verbosity -> FilePath -> IO ()
run v f =
  readFile f >>= \s ->
  let lexemes = myLexer s in
  case pProgram lexemes of

    Bad err -> do
      putStrV v "Tokens:"
      putStrV v $ show lexemes
      putError err

    Ok tree -> do
      putStrV v "Parse completed"
      showTree v tree

      runTypeControl tree
      putStrV v "Type control completed\n"

      tree <- runStaticEvaluation tree
      putStrV v "Static evaluation completed"
      showTree v tree

      tree <- runReturnEvaluation tree
      putStrV v "Return optimisation completed"
      showTree v tree

      tree <- runStringEvaluation tree
      putStrV v "String evaluation completed"
      showTree v tree

      code <- runIntermediateCodeGeneration (void tree)
      putStrV v "Code generation completed"
      putStrV v (show code)

      code <- optimise code
      putStrV v "Optimisations completed"
      putStrV v (show code)

      asm <- generateAssembly code
      putStrV v asm

      generateFile f asm
      putStrLn "OK"
      exitSuccess
