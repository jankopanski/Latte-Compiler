module Frontend.Globals where

import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure)

-- Data structures --

type Position = Maybe (Int, Int)

-- Functions --

putError :: String -> IO b
putError err = hPutStrLn stderr "ERROR" >> hPutStrLn stderr err >> exitFailure

printError :: Show a => a -> IO b
printError = putError . show

showPosition :: Position -> String
showPosition Nothing = ""
showPosition (Just (line, column)) = "line " ++ show line ++ ", column " ++ show column

showErrorPosition :: Position -> String
showErrorPosition pos = showPosition pos ++ ": "
