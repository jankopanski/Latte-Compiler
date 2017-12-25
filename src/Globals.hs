module Globals where

-- Data structures --

type Position = Maybe (Int, Int)

-- Functions --

showPosition :: Position -> String
showPosition Nothing = ""
showPosition (Just (line, column)) = "line " ++ show line ++ ", position " ++ show column

showErrorPosition :: Position -> String
showErrorPosition pos = showPosition pos ++ ": "
