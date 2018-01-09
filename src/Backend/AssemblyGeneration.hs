module Backend.AssemblyGeneration where

import System.FilePath (replaceExtension)

import Parser.AbsLatte
import Backend.IntermediateCode

generateAssembly :: Code -> IO String
generateAssembly code = return $ unlines $
  concatMap generateString (strings code) ++
  concatMap generateFunction (functions code)

generateFunction :: (Ident, [Instruction]) -> [String]
generateFunction (Ident s, ins) = ("\t.globl " ++ s) : (s ++ ":") : map show ins

generateString :: (String, Label) -> [String]
generateString (string, label) = [show label ++ ":", "\t.string " ++ show string]

generateFile :: FilePath -> String -> IO ()
generateFile file = writeFile (replaceExtension file "s")
