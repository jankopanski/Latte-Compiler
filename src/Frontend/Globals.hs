module Frontend.Globals where

import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure)

import Parser.AbsLatte


type Position = Maybe (Int, Int)

inbuildFunctions :: [TopDef Position]
inbuildFunctions =
  [
  FnDef Nothing (Void Nothing) (Ident "printInt")
    [Arg Nothing (Int Nothing) (Ident "")] (Block Nothing []),
  FnDef Nothing (Void Nothing) (Ident "printString")
    [Arg Nothing (Str Nothing) (Ident "")] (Block Nothing []),
  FnDef Nothing (Void Nothing) (Ident "error") [] (Block Nothing []),
  FnDef Nothing (Int Nothing) (Ident "readInt") [] (Block Nothing []),
  FnDef Nothing (Str Nothing) (Ident "readString") [] (Block Nothing []),
  FnDef Nothing (Str Nothing) (Ident "_allocString")
    [Arg Nothing (Str Nothing) (Ident ""), Arg Nothing (Int Nothing) (Ident "")]
    (Block Nothing []),
  FnDef Nothing (Str Nothing) (Ident "_concatString")
    [Arg Nothing (Str Nothing) (Ident ""), Arg Nothing (Str Nothing) (Ident "")]
    (Block Nothing []),
  FnDef Nothing (Bool Nothing) (Ident "_cmpString")
    [Arg Nothing (Str Nothing) (Ident ""), Arg Nothing (Str Nothing) (Ident "")]
    (Block Nothing [])
  ]

putError :: String -> IO b
putError err = hPutStrLn stderr "ERROR" >> hPutStrLn stderr err >> exitFailure

printError :: Show a => a -> IO b
printError = putError . show

showPosition :: Position -> String
showPosition Nothing = ""
showPosition (Just (line, column)) = "line " ++ show line ++ ", column " ++ show column

showErrorPosition :: Position -> String
showErrorPosition pos = showPosition pos ++ ": "
