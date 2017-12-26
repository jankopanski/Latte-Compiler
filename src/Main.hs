module Main where

import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import Control.Monad (when)

import ParLatte
import PrintLatte
import ErrM

import TypeControl (checkTypes)
import ExpressionEvaluation (evalProgram)
import ReturnEvaluation (returnEvalProgram)

type Verbosity = Int

putStrV :: Verbosity -> String -> IO ()
putStrV v s = when (v > 1) $ putStrLn s

showTree :: (Show a, Print a) => Int -> a -> IO ()
showTree v tree = do
  putStrV v $ "\n[Abstract Syntax]\n\n" ++ show tree
  putStrV v $ "\n[Linearized tree]\n\n" ++ printTree tree

usage :: IO ()
usage = putStrLn $ unlines
  [ "usage: Call with one of the following argument combinations:"
  , "  --help         Display this help message."
  , "  (file)         Parse content of files verbosely."
  , "  -s (file)      Silent mode. Parse content of files silently."
  ]

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> usage >> exitFailure
    ["--help"] -> usage >> exitSuccess
    "-s":[f] -> run 0 f
    [f] -> run 2 f
    _ -> putStrLn "Invalid flag or number of files - one file allowed only"

run :: Verbosity -> FilePath -> IO ()
run v f =
  readFile f >>= \s ->
  let lexemes = myLexer s in
  case pProgram lexemes of
    Bad err -> do
      putStrLn "Syntax error\n"
      putStrV v "Tokens:"
      putStrV v $ show lexemes
      putStrLn err
      exitFailure
    Ok tree -> do
      putStrV v "Parse completed"
      showTree v tree
      checkTypes tree
      putStrV v "Type check completed"
      let optTree = evalProgram tree
      putStrV v "Static evaluation completed"
      showTree v optTree
      retOptTree <- returnEvalProgram optTree
      putStrV v "Return optimisation completed"
      showTree v retOptTree
      exitSuccess
