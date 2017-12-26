module Globals where

-- Data structures --

type Position = Maybe (Int, Int)

-- Functions --

showPosition :: Position -> String
showPosition Nothing = ""
showPosition (Just (line, column)) = "line " ++ show line ++ ", column " ++ show column

showErrorPosition :: Position -> String
showErrorPosition pos = "error at " ++ showPosition pos ++ ": "
